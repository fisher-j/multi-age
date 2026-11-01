"""Validate publication exports and current outputs without historical snapshots.

--protected optionally verifies original thesis inputs against export provenance.
Normal publication checks never read thesis objects or activate thesis renv.
"""
import argparse
import csv
import hashlib
import json
import math
from pathlib import Path

PROJECT = Path(__file__).resolve().parents[1]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def validate_exports(project, protected=False):
    root = project / "inputs/thesis"
    manifest = json.loads((root / "manifest.json").read_text(encoding="utf-8"))
    errors = []
    for name, checksum in manifest["exports"].items():
        path = root / name
        if not path.is_file() or sha(path) != checksum:
            errors.append(f"Export missing or modified: {name}")
    if protected:
        for name, checksum in manifest["inputs"].items():
            path = project.parent / "analysis" / name
            if not path.is_file() or sha(path) != checksum:
                errors.append(f"Original thesis input changed or missing: {name}")
    return errors


def validate_outputs(project, outputs, domains, require_models=False, task_id=None):
    errors = []
    tasks = json.loads((project / "tasks.json").read_text(encoding="utf-8"))
    required = set()
    for task in tasks:
        if task_id and task["id"] != task_id:
            continue
        for field in (("outputs",) if task_id else ("files", "outputs", "inputs")):
            for name in task.get(field, []):
                prefix = "publication/outputs/"
                if name.startswith(prefix):
                    relative = Path(name[len(prefix):])
                    if relative.parts[0] in domains:
                        required.add(relative)
    for relative in sorted(required):
        path = outputs / relative
        if not path.is_file() or not path.stat().st_size:
            errors.append(f"Declared output missing or empty: {relative.as_posix()}")
    for domain in domains:
        domain_root = outputs / domain
        if not domain_root.is_dir():
            errors.append(f"Output directory missing: {domain}")
            continue
        for path in sorted(domain_root.rglob("*.csv")):
            relative = path.relative_to(outputs).as_posix()
            try:
                with path.open(encoding="utf-8-sig", newline="") as stream:
                    rows = list(csv.reader(stream, strict=True))
                if not rows or not rows[0] or any(not name for name in rows[0]) or len(set(rows[0])) != len(rows[0]):
                    errors.append(f"Invalid CSV header: {relative}")
                    continue
                headers = rows[0]
                for row_number, values in enumerate(rows[1:], start=2):
                    if len(values) != len(headers):
                        errors.append(f"CSV row width differs from header: {relative}:{row_number}")
                        continue
                    row = dict(zip(headers, values))
                    if {"estimate", "lcl", "ucl"} <= set(headers):
                        fields = ["estimate", "lcl", "ucl"] + (["se"] if "se" in headers else [])
                        numeric = {name: float(row[name]) for name in fields}
                        if not all(math.isfinite(value) for value in numeric.values()):
                            errors.append(f"Non-finite estimate or interval: {relative}:{row_number}")
                        elif not numeric["lcl"] <= numeric["estimate"] <= numeric["ucl"]:
                            errors.append(f"Interval does not contain estimate: {relative}:{row_number}")
                        elif "se" in numeric and numeric["se"] < 0:
                            errors.append(f"Negative standard error: {relative}:{row_number}")
                    if require_models and (row.get("fit_status") == "fit" or
                                           path.name.endswith("-fit-register.csv")):
                        model = path.parent.parent / "models" / (row["model_id"] + ".rds")
                        if not model.is_file():
                            errors.append(f"Fitted model missing: {model.relative_to(outputs).as_posix()}")
            except (csv.Error, UnicodeError, ValueError, KeyError) as error:
                errors.append(f"Invalid output CSV {relative}: {error}")
    return errors


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--inputs-only", action="store_true")
    parser.add_argument("--protected", action="store_true")
    parser.add_argument("--require-models", action="store_true")
    parser.add_argument("--task", choices=("F01", "RD04", "RD02"), help="Check only this task's current outputs")
    parser.add_argument("--outputs", type=Path, default=PROJECT / "outputs")
    parser.add_argument("--domains", default="fuel,regeneration")
    parser.add_argument("--report", type=Path)
    args = parser.parse_args()
    domains = args.domains.split(",")
    if not domains or any(domain not in ("fuel", "regeneration") for domain in domains):
        parser.error("--domains must contain fuel and/or regeneration")
    if args.task and domains != (["regeneration"] if args.task == "RD02" else ["fuel"]):
        parser.error("--task must match --domains")
    errors = validate_exports(PROJECT, args.protected) if args.inputs_only or args.protected else []
    if not args.inputs_only:
        errors.extend(validate_outputs(PROJECT, args.outputs.resolve(), domains, args.require_models, args.task))
    result = dict(valid=not errors, errors=errors, domains=domains,
                  outputs=str(args.outputs.resolve()), required_model_check=args.require_models)
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8", newline="\n")
    print(f"Publication validation: {len(errors)} failures.")
    if errors:
        print(json.dumps(errors[:8], indent=2))
        raise SystemExit(1)


if __name__ == "__main__":
    main()
