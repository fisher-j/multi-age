"""Quarto lifecycle/artifact checks. Never executes analytical documents or chunks."""
import argparse
import fnmatch
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

PROJECT = Path(__file__).resolve().parents[1]


def read(path):
    return json.loads(path.read_text(encoding="utf-8"))


def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def module(name):
    spec = importlib.util.spec_from_file_location(name, PROJECT / "scripts" / (name + ".py"))
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def relative(name):
    return name.removeprefix("publication/")


def inventory(task):
    return task.get("outputs", []) + task.get("private_outputs", [])


def owner(name, tasks):
    matches = [task["id"] for task in tasks if any(
        fnmatch.fnmatchcase(relative(name), relative(pattern)) for pattern in inventory(task))]
    if len(matches) > 1:
        raise ValueError(f"Multiple artifact owners: {name}: {matches}")
    return matches[0] if matches else None


def resolve(name, tasks, selection, run, project=None):
    project = project or PROJECT
    name = relative(name)
    if name.startswith("analysis/"):
        return project.parent / name
    if owner(name, tasks) in selection:
        return run / name
    return project / name


def task_files(task, base):
    paths = set()
    for pattern in inventory(task):
        paths.update(path for path in base.glob(relative(pattern)) if path.is_file())
    return paths


def context():
    return read(PROJECT / ".runs/active-render.json")


def run_state():
    ctx = context()
    if ctx["mode"] != "recompute":
        raise ValueError("No active recompute render")
    run = Path(ctx["run"])
    return run, read(run / "execution.json")


def pre():
    profiles = os.getenv("QUARTO_PROFILE", "").split(",")
    checks = module("check-analysis")
    errors = checks.validate_exports(PROJECT)
    if errors:
        raise ValueError("; ".join(errors))
    inputs = os.getenv("QUARTO_PROJECT_INPUT_FILES", "").splitlines()
    if "recompute" not in profiles:
        write(PROJECT / ".runs/active-render.json", dict(mode="review"))
    else:
        output = (PROJECT / os.environ["QUARTO_PROJECT_OUTPUT_DIR"]).resolve()
        runs = (PROJECT / ".runs").resolve()
        if not output.is_relative_to(runs) or output.parent == runs or output.name != "site":
            raise ValueError("Recompute output must be .runs/<run-name>/site")
        run = output.parent
        if (run / "execution.json").exists():
            raise ValueError(f"Run already exists; inspect/archive it before reuse: {run}")
        rbin = Path(os.environ["QUARTO_R"])
        subprocess.run([str(rbin / "Rscript.exe"), "--vanilla", "scripts/verify-environment.R"],
                       cwd=PROJECT, check=True)
        write(PROJECT / ".runs/active-render.json", dict(mode="recompute", run=str(run)))
        write(run / "execution.json", dict(status="awaiting_selection", selected=[], completed=[],
            documents=inputs, lock_sha256=sha(PROJECT / "renv.lock"),
            inputs_sha256=sha(PROJECT / "inputs/thesis/manifest.json")))
    module("prepare-workflow").prepare()


def select():
    run, state = run_state()
    selection = read(run / "selection.json")
    tasks = read(PROJECT / "tasks.json")
    supported = {t["id"] for t in tasks if t.get("recomputable")}
    if not isinstance(selection, list) or any(item not in supported for item in selection):
        raise ValueError(f"Unsupported recompute selection: {selection}")
    documents = {Path(path).name for path in state["documents"]}
    for task_id in selection:
        if f"task-{task_id.lower()}.qmd" not in documents:
            raise ValueError(f"Selected task {task_id} is not a render target")
    # Validate saved upstream prerequisites before any calculation begins.
    for task in tasks:
        if task["id"] not in selection:
            continue
        for name in task.get("inputs", []):
            if owner(name, tasks) not in selection and not resolve(name, tasks, selection, run).is_file():
                raise ValueError(f"Missing saved prerequisite for {task['id']}: {name}")
        if task["id"] == "RD04" and "F01" not in selection:
            registry = PROJECT / "outputs/fuel/manifests/model-comparison.csv"
            import csv
            with registry.open(encoding="utf-8-sig", newline="") as stream:
                models = [row["model_id"] for row in csv.DictReader(stream) if row["fit_status"] == "fit"]
            for model in models:
                if not (PROJECT / f"outputs/fuel/models/{model}.rds").is_file():
                    raise ValueError(f"RD04 needs saved F01 model {model}; explicitly select F01 to refit it")
    state.update(selected=selection, status="running")
    write(run / "execution.json", state)


def fingerprints(task, tasks, selection, run):
    result = {}
    for pattern in task.get("inputs", []) + task.get("private_inputs", []):
        # Input patterns are expanded against their producer's active root.
        # Expand only the active producer's inventory, never a union with old caches.
        base = run if owner(pattern, tasks) in selection else PROJECT
        if pattern.startswith("analysis/"):
            base = PROJECT.parent
        names = {p.relative_to(base).as_posix() for p in base.glob(relative(pattern)) if p.is_file()}
        if not names and not any(char in pattern for char in "*?["):
            raise ValueError(f"Required upstream artifact missing: {pattern}")
        for name in sorted(names):
            path = resolve(name, tasks, selection, run)
            if not path.is_file():
                raise ValueError(f"Required upstream artifact missing: {path}")
            result[name] = sha(path)
    return result


def begin(task_id):
    run, state = run_state()
    if task_id not in state["selected"] or task_id in state["completed"]:
        raise ValueError(f"Task not selected or already completed: {task_id}")
    tasks = read(PROJECT / "tasks.json")
    task = next(t for t in tasks if t["id"] == task_id)
    for dependency in task.get("depends_on", []):
        if dependency in state["selected"] and dependency not in state["completed"]:
            raise ValueError(f"Selected upstream task {dependency} has not completed")
    state.update(active_task=task_id)
    session = run / f"{task_id}-r-session.json"
    if session.exists():
        state.setdefault("sessions", {})[task_id] = read(session)
    write(run / "execution.json", state)
    write(run / f"{task_id}-inputs.json", fingerprints(task, tasks, state["selected"], run))


def complete(task_id):
    run, state = run_state()
    if state.get("active_task") != task_id:
        raise ValueError(f"Task completion out of order: {task_id}")
    tasks = read(PROJECT / "tasks.json")
    task = next(t for t in tasks if t["id"] == task_id)
    for pattern in inventory(task):
        if pattern == task["execution_manifest"]:
            continue
        if not list(run.glob(relative(pattern))):
            raise ValueError(f"Task {task_id} did not produce {pattern}")
    manifest = dict(task=task_id, input_sha256=read(run / f"{task_id}-inputs.json"),
                    source_sha256={name: sha(PROJECT / relative(name)) for name in task["files"]},
                    outputs=[p.relative_to(run).as_posix() for p in sorted(task_files(task, run))])
    manifest["r_session"] = state.get("sessions", {}).get(task_id)
    write(run / relative(task["execution_manifest"]), manifest)
    state["completed"].append(task_id)
    state.pop("active_task", None)
    write(run / "execution.json", state)


def review(task_id):
    tasks = read(PROJECT / "tasks.json")
    task = next(t for t in tasks if t["id"] == task_id)
    ctx = context()
    run = Path(ctx["run"]) if ctx["mode"] == "recompute" else None
    state = read(run / "execution.json") if run else {"selected": []}
    selected = state["selected"]
    manifest_path = resolve(task["execution_manifest"], tasks, selected, run)
    mode = "Recomputed in this render" if task_id in selected else "Reviewing saved outputs; no recalculation"
    print(f"**Execution: {mode}.**")
    if not manifest_path.exists():
        print("Input fingerprints have not yet been recorded for these saved outputs.")
        return
    manifest = read(manifest_path)
    saved = manifest["input_sha256"]
    current = fingerprints(task, tasks, selected, run)
    unavailable = sorted(name for name in saved if name not in current
                         and name.startswith("outputs/fuel/models/"))
    changed = sorted(name for name in saved.keys() | current.keys()
                     if name not in unavailable and saved.get(name) != current.get(name))
    if changed:
        print("\n**Upstream inputs have changed since these outputs were calculated.** "
              "The saved evidence remains available; recomputation and scientific acceptance are separate decisions.")
        for name in changed:
            print(f"- `{name}`")
    elif not unavailable:
        print("Recorded input fingerprints match the active upstream artifacts.")
    if unavailable:
        print("Saved model fingerprints cannot be checked because local fitted-model caches are unavailable. "
              "The saved tables and figures remain reviewable.")
    changed_sources = [name for name, checksum in manifest.get("source_sha256", {}).items()
                       if sha(PROJECT / relative(name)) != checksum]
    if changed_sources:
        print("\nMaintained sources differ from the recorded producing versions. "
              "The saved evidence has not been recalculated from these edits.")
        for name in changed_sources:
            print(f"- `{name}`")


def progress(label):
    run, state = run_state()
    state["active_step"] = label
    state.setdefault("executed_steps", []).append(label)
    write(run / "execution.json", state)


def failed():
    """Called by the environment wrapper when Quarto exits unsuccessfully."""
    if not (PROJECT / ".runs/active-render.json").exists() or context()["mode"] != "recompute":
        return
    run, state = run_state()
    if state["status"] in ("running", "awaiting_selection"):
        state["status"] = "render_failed"
        write(run / "execution.json", state)


def promote(files, run, project=PROJECT):
    """Preflight copies, then replace; roll back replacements on an I/O failure."""
    pending, previous, replaced = [], {}, []
    try:
        for source, target in files:
            if not source.is_file():
                raise ValueError(f"Missing promotion source: {source}")
            if not target.resolve().is_relative_to(project.resolve()):
                raise ValueError(f"Promotion target outside project: {target}")
            target.parent.mkdir(parents=True, exist_ok=True)
            candidate = target.with_name(target.name + ".publication-pending")
            pending.append((candidate, target))
            shutil.copy2(source, candidate)
            if target.exists():
                backup = run / "promotion-backup" / target.relative_to(project)
                backup.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(target, backup)
                previous[target] = backup
        for candidate, target in pending:
            candidate.replace(target)
            replaced.append(target)
    except OSError:
        for target in reversed(replaced):
            if target in previous:
                shutil.copy2(previous[target], target)
            else:
                target.unlink()
        raise
    finally:
        for candidate, _ in pending:
            candidate.unlink(missing_ok=True)


def post():
    ctx = context()
    if ctx["mode"] != "recompute":
        return
    run, state = run_state()
    if state["status"] != "running" or set(state["selected"]) != set(state["completed"]):
        raise ValueError("Selected tasks did not all complete; canonical artifacts preserved")
    tasks = read(PROJECT / "tasks.json")
    # A validation-only union view never supplies fallbacks for selected producers.
    view = run / "validation-view"
    for task in tasks:
        base = run if task["id"] in state["selected"] else PROJECT
        for source in task_files(task, base):
            target = view / source.relative_to(base)
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
    checks = module("check-analysis")
    errors = checks.validate_exports(PROJECT)
    if sha(PROJECT / "renv.lock") != state["lock_sha256"]:
        errors.append("Publication lock changed during recomputation")
    for task in tasks:
        if task["id"] in state["selected"]:
            if fingerprints(task, tasks, state["selected"], run) != read(run / f"{task['id']}-inputs.json"):
                errors.append(f"Inputs changed while computing {task['id']}")
    domains = sorted({Path(relative(name)).parts[1] for task in tasks
                      if task.get("recomputable") and (not state["selected"] or task["id"] in state["selected"])
                      for name in inventory(task) if relative(name).startswith("outputs/")})
    errors += checks.validate_outputs(PROJECT, view / "outputs", domains, bool(state["selected"]))
    write(run / "validation.json", dict(valid=not errors, errors=errors))
    if errors:
        state["status"] = "validation_failed"
        write(run / "execution.json", state)
        raise ValueError("; ".join(errors[:10]))
    # Refresh downloads in staged HTML against the very same active artifact view.
    bundle = Path(os.environ["QUARTO_PROJECT_OUTPUT_DIR"])
    if not bundle.is_absolute():
        bundle = PROJECT / bundle
    bundle = bundle / "repo-files"
    source_manifest = read(PROJECT / "repo-files/manifest.json")
    for entry in source_manifest:
        source = resolve(entry.get("source", entry["path"]), tasks, state["selected"], run)
        target = bundle / entry["path"]
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
        entry["sha256"] = sha(source)
    write(bundle / "manifest.json", source_manifest)
    files = []
    for task in tasks:
        if task["id"] in state["selected"]:
            files.extend((p, PROJECT / p.relative_to(run)) for p in task_files(task, run))
    if "RD04" in state["selected"]:
        files.append((run / "outputs/fuel/figures/rd04-fuel-phase-pct-inventory.png",
                      PROJECT / "assets/rd04-fuel-phase-pct-inventory.png"))
    promote(files, run)
    state["status"] = "verified_and_promoted"
    write(run / "execution.json", state)
    print(f"Validated and promoted selected tasks: {state['selected']}; report: {run}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=["pre", "select", "begin", "complete", "review", "progress", "post", "failed"])
    parser.add_argument("task", nargs="?")
    args = parser.parse_args()
    try:
        {"pre": pre, "select": select, "post": post, "failed": failed}.get(args.action, lambda: globals()[args.action](args.task))()
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        raise SystemExit(str(error))
