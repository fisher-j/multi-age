"""Check rendered publication navigation, local links, anchors, and downloads."""

import csv
import json
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit


class TableContent(HTMLParser):
    """Read DT's escaped cells and column headings as browser-visible text."""
    def __init__(self, markup):
        super().__init__()
        self.text = []
        self.headings = []
        self.heading = False
        self.formula_subscript = False
        self.feed(markup)

    def handle_starttag(self, tag, attrs):
        if tag == "th":
            self.heading = True
            self.headings.append("")
        if tag == "sub":
            self.formula_subscript = True

    def handle_endtag(self, tag):
        if tag == "th":
            self.heading = False

    def handle_data(self, data):
        self.text.append(data)
        if self.heading:
            self.headings[-1] += data


class Document(HTMLParser):
    def __init__(self, path):
        super().__init__()
        self.ids = set()
        self.links = []
        self.sections = []
        self.register_links = []
        self.formula_errors = []
        self.review_source = None
        self.in_cell = False
        self.div_ids = []
        self.result_tables = {}
        self.result_table = None
        self.widget_ids = set()
        self.widget_script = None
        self.widgets = []
        self.review_tables = {}
        self.widget_errors = []
        self.details = []
        self.details_by_id = {}
        self.images_in_details = {}
        self.table_details = {}
        self.feed(path.read_text(encoding="utf-8"))

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "details":
            self.details.append(attrs)
            if "id" in attrs:
                self.details_by_id[attrs["id"]] = attrs
        if tag == "img":
            self.images_in_details[attrs.get("src", "")] = tuple(self.details)
        if tag == "div":
            self.div_ids.append(attrs.get("id", ""))
            if "datatables" in attrs.get("class", "").split():
                self.widget_ids.add(attrs["id"])
        if tag == "script" and attrs.get("data-for") in self.widget_ids:
            self.widget_script = []
        if tag == "table":
            self.result_table = next((id for id in reversed(self.div_ids) if id.startswith("tbl-")), None)
            if self.result_table:
                self.result_tables[self.result_table] = []
        if tag == "tr" and self.result_table:
            self.result_tables[self.result_table].append([])
        if tag == "details" and "data-review-source" in attrs:
            self.review_source = attrs["data-review-source"]
        if tag in ("td", "th"):
            self.in_cell = True
            if self.result_table:
                self.result_tables[self.result_table][-1].append("")
        # A previous Markdown rendering bug turned literal formula tildes into
        # subscript markup. CSV reviews must display those formulas literally.
        if tag == "sub" and self.in_cell and (self.review_source or "").endswith(".csv"):
            self.formula_errors.append(self.review_source)
        if tag == "section":
            self.sections.append(attrs.get("id"))
        if "id" in attrs:
            self.ids.add(attrs["id"])
        if tag == "a" and "task-register" in self.sections and "href" in attrs:
            self.register_links.append(attrs["href"])
        for field in ("href", "src"):
            if field in attrs:
                self.links.append(attrs[field])

    def handle_endtag(self, tag):
        if tag == "script" and self.widget_script is not None:
            payload = json.loads("".join(self.widget_script))["x"]
            self.widgets.append(payload)
            header = TableContent(payload["container"]).headings
            rows = [header]
            for values in zip(*payload["data"]):
                row = []
                for value in values:
                    cell = TableContent("" if value is None else str(value))
                    row.append("".join(cell.text))
                    if (self.review_source or "").endswith(".csv") and cell.formula_subscript:
                        self.formula_errors.append(self.review_source)
                rows.append(row)
            label = next((id for id in reversed(self.div_ids) if id.startswith("tbl-")), None)
            if label:
                self.result_tables[label] = rows
                self.table_details[label] = tuple(self.details)
            if (self.review_source or "").endswith(".csv"):
                self.review_tables[self.review_source] = rows
            options = payload["options"]
            if not (options.get("ordering") and options.get("searching") and
                    "Buttons" in payload.get("extensions", []) and
                    any(button.get("extend") == "colvis" for button in options.get("buttons", []))):
                self.widget_errors.append("DT table missing sorting, search, or field visibility controls")
            self.widget_script = None
        if tag == "div":
            self.div_ids.pop()
        if tag == "table":
            self.result_table = None
        if tag == "details":
            self.review_source = None
            self.details.pop()
        if tag in ("td", "th"):
            self.in_cell = False
        if tag == "section" and self.sections:
            self.sections.pop()

    def handle_data(self, data):
        if self.widget_script is not None:
            self.widget_script.append(data)
        if self.result_table and self.in_cell:
            self.result_tables[self.result_table][-1][-1] += data


def rd02_review_errors(doc):
    errors = []
    for id in ("rd02-historical-details", "rd02-comparison-details"):
        if id not in doc.details_by_id or "open" in doc.details_by_id[id]:
            errors.append(f"{id} must exist and start collapsed")
    for name in ("rd02-regeneration-basal-area.png", "rd02-overall-sensitivity.png"):
        images = [parents for src, parents in doc.images_in_details.items() if src.endswith(name)]
        if len(images) != 1 or images[0]:
            errors.append(f"{name} must be visible outside folded details")
    if doc.table_details.get("tbl-rd02-selected-species-estimates"):
        errors.append("Publication estimates table must remain unfolded")
    return errors


def check():
    project = Path(__file__).resolve().parents[1]
    output = project.parent / "docs" / "publication"
    tasks = json.loads((project / "tasks.json").read_text(encoding="utf-8"))
    documents = {}
    errors = []
    for source in project.glob("*.qmd"):
        path = output / source.with_suffix(".html").name
        if not path.is_file():
            errors.append(f"Missing rendered page: {path.name}")
        else:
            documents[path] = Document(path)
    for page, doc in list(documents.items()):
        errors.extend(f"{page.name}: {error}" for error in doc.widget_errors)
        for source, rows in doc.review_tables.items():
            with (project.parent / source).open(encoding="utf-8-sig", newline="") as stream:
                expected = list(csv.reader(stream))
                model_ids = [i for i, name in enumerate(expected[0]) if name in
                             ("model_id", "component_model_id", "provisional_aic_leader", "Provisional leader")]
                for row in expected[1:]:
                    for i in model_ids:
                        row[i] = row[i].replace("_", " ")
                if rows != expected:
                    errors.append(f"{page.name}: DT review differs from saved CSV: {source}")
        for source in doc.formula_errors:
            errors.append(f"{page.name}: formula rendered as subscript in CSV review: {source}")
        for link in doc.links:
            url = urlsplit(link)
            if url.scheme or url.netloc or link.startswith("/"):
                continue
            target = (page.parent / unquote(url.path)).resolve() if url.path else page
            if target.name.lower().endswith((".rmd", ".qmd", ".rmd.txt", ".qmd.txt")):
                errors.append(f"{page.name}: analytical source download link remains: {link}")
            if target.is_dir():
                target /= "index.html"
            if not target.is_file():
                errors.append(f"{page.name}: missing target {link}")
                continue
            if url.fragment and target.suffix == ".html":
                if target not in documents:
                    documents[target] = Document(target)
                if unquote(url.fragment) not in documents[target].ids:
                    errors.append(f"{page.name}: missing anchor {link}")
    for task in tasks:
        key = task["id"].lower()
        page = output / f"task-{key}.html"
        if page in documents:
            for part in ("files", "record"):
                if f"{key}-{part}" not in documents[page].ids:
                    errors.append(f"{page.name}: missing task section {part}")
        task_list = documents.get(output / "task-list.html")
        if task_list and (key not in task_list.ids or f"#{key}" not in task_list.register_links):
            errors.append(f"Task register must link to the task list entry: {key}")
    # A table caption around echoed source is not a rendered result. Check the
    # actual table cells in both the task page and its manuscript consumers.
    for label, relative, producer in (
        ("tbl-rd02-selected-species-estimates",
         "regeneration/selected-species-models/tables/rd02-selected-species-display.csv", "task-rd02.html"),
        ("tbl-rd04-fuel-estimates", "fuel/tables/rd04-fuel-display.csv", "task-rd04.html"),
    ):
        with (project / "outputs" / relative).open(encoding="utf-8", newline="") as stream:
            expected = list(csv.reader(stream))
        for page in (producer, "draft-supplement.html", "manuscript-full.html"):
            doc = documents.get(output / page)
            if doc and doc.result_tables.get(label) != expected:
                errors.append(f"{page}: rendered table cells differ from saved CSV: {label}")
    rd02 = documents.get(output / "task-rd02.html")
    if rd02:
        errors.extend(f"task-rd02.html: {error}" for error in rd02_review_errors(rd02))
    if rd02 and len(rd02.result_tables.get("tbl-rd02-random-effects-fit", [])) != 9:
        errors.append("task-rd02.html: sensitivity table must render all eight model rows")
    import importlib.util
    spec = importlib.util.spec_from_file_location("workflow", project / "scripts/prepare-workflow.py")
    workflow = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(workflow)
    downloads = workflow.linked_downloads()
    for relative in downloads:
        copied, source = output / "repo-files" / relative, project.parent / relative
        if not copied.is_file() or copied.read_bytes() != source.read_bytes():
            errors.append(f"Download missing or differs from repository: {relative}")
    for path in (output / "repo-files").rglob("*"):
        if path.is_file() and path.relative_to(output / "repo-files").as_posix() not in downloads:
            errors.append(f"Obsolete download: {path.relative_to(output)}")
    for path in output.rglob("*"):
        if path.is_file() and path.name.lower().endswith((".rmd", ".qmd", ".rmd.txt", ".qmd.txt")):
            errors.append(f"Analytical source document remains published: {path.relative_to(output)}")
    if errors:
        raise SystemExit("\n".join(sorted(set(errors))))
    print(f"Checked {len(documents)} pages, task-section anchors, local links, "
          f"{len(downloads)} current downloads, rendered tables, DT controls, and formula escaping.")


if __name__ == "__main__":
    check()
