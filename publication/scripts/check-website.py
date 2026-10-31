"""Check rendered publication navigation, local links, anchors, and downloads."""

import csv
import hashlib
import json
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit


class Document(HTMLParser):
    def __init__(self, path):
        super().__init__()
        self.ids = set()
        self.links = []
        self.sections = []
        self.register_links = []
        self.review_sources = {}
        self.review_source = None
        self.in_code = False
        self.in_cell = False
        self.feed(path.read_text(encoding="utf-8"))

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "details" and "data-review-source" in attrs:
            self.review_source = attrs["data-review-source"]
            self.review_sources[self.review_source] = {
                "code": "", "rows": [], "collapsed": "open" not in attrs,
            }
        if self.review_source:
            review = self.review_sources[self.review_source]
            if tag == "code":
                self.in_code = True
            if tag == "tr":
                review["rows"].append([])
            if tag in ("td", "th"):
                self.in_cell = True
                review["rows"][-1].append("")
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
        if tag == "details":
            self.review_source = None
        if tag == "code":
            self.in_code = False
        if tag in ("td", "th"):
            self.in_cell = False
        if tag == "section" and self.sections:
            self.sections.pop()

    def handle_data(self, data):
        if self.review_source:
            review = self.review_sources[self.review_source]
            if self.in_code:
                review["code"] += data
            if self.in_cell:
                review["rows"][-1][-1] += data


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
        for link in doc.links:
            url = urlsplit(link)
            if url.scheme or url.netloc or link.startswith("/"):
                continue
            target = (page.parent / unquote(url.path)).resolve() if url.path else page
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
            if task.get("inline_review", False):
                reviews = documents[page].review_sources
                expected = {
                    path for path in task["files"] + task.get("outputs", [])
                    if path.endswith((".R", ".csv"))
                }
                if set(reviews) != expected:
                    errors.append(f"{task['id']}: inline review must include exactly its maintained R scripts and produced CSVs")
                for relative, review in reviews.items():
                    source = project.parent / relative
                    if not review["collapsed"]:
                        errors.append(f"{task['id']}: review should start collapsed: {relative}")
                    if source.suffix == ".R":
                        if review["code"].rstrip("\n") != source.read_text(encoding="utf-8").rstrip("\n"):
                            errors.append(f"{task['id']}: rendered code differs from source: {relative}")
                    else:
                        with source.open(encoding="utf-8-sig", newline="") as stream:
                            rows = list(csv.reader(stream))
                        # HTML collapses whitespace, including indentation in
                        # saved R formulas. Compare every cell's displayed text.
                        displayed = lambda table: [[" ".join(cell.split()) for cell in row] for row in table]
                        if displayed(review["rows"]) != displayed(rows):
                            errors.append(f"{task['id']}: rendered CSV cells differ from source: {relative}")
        task_list = documents.get(output / "task-list.html")
        if task_list and (key not in task_list.ids or f"#{key}" not in task_list.register_links):
            errors.append(f"Task register must link to the task list entry: {key}")
    manifest = json.loads((output / "repo-files/manifest.json").read_text(encoding="utf-8"))
    registered = {item["path"] for item in manifest} | {"manifest.json"}
    for path in (output / "repo-files").rglob("*"):
        if path.is_file() and path.relative_to(output / "repo-files").as_posix() not in registered:
            errors.append(f"Unregistered download remains published: {path.relative_to(output)}")
    for item in manifest:
        copied = output / "repo-files" / item["path"]
        if not copied.is_file() or hashlib.sha256(copied.read_bytes()).hexdigest() != item["sha256"]:
            errors.append(f"Download differs from recorded source: {item['path']}")
        source = project.parent / item["path"]
        if hashlib.sha256(source.read_bytes()).hexdigest() != item["sha256"]:
            errors.append(f"Download is stale relative to local repository: {item['path']}")
    if errors:
        raise SystemExit("\n".join(sorted(set(errors))))
    reviews = ", ".join(
        f"{task['id']}: {len(documents[output / ('task-' + task['id'].lower() + '.html')].review_sources)}"
        for task in tasks if task.get("inline_review", False)
    )
    print(f"Checked {len(documents)} pages, task-section anchors, local links, "
          f"{len(manifest)} current downloads, and complete inline source reviews ({reviews}).")


if __name__ == "__main__":
    check()
