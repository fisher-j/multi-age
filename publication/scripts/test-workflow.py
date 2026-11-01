"""Repository download preparation regressions."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from contextlib import redirect_stdout
from io import StringIO
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("workflow", Path(__file__).with_name("prepare-workflow.py"))
workflow = importlib.util.module_from_spec(spec)
spec.loader.exec_module(workflow)


class WorkflowTests(unittest.TestCase):
    def test_source_documents_are_listed_without_downloads_and_old_copies_are_pruned(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            project = root / "publication"
            project.mkdir()
            sources = ["publication/task-f01.qmd", "publication/history.Rmd"]
            for name in sources:
                (root / name).write_text("Historical analytical source")
            csv = project / "estimates.csv"
            csv.write_text("estimate\n1\n")
            (project / "tasks.json").write_text(json.dumps([dict(id="F01", title="Fuel", status="Review",
                target="Figure", summary="Review", files=sources + ["publication/estimates.csv"])]))
            for bundle in (project / "repo-files", root / "docs/publication/repo-files"):
                bundle.mkdir(parents=True)
                for name in ("old.qmd", "old.qmd.txt", "old.Rmd"):
                    (bundle / name).write_text("Old source download")
            with patch.object(workflow, "PROJECT", project), patch.object(workflow, "REPOSITORY", root), \
                    redirect_stdout(StringIO()):
                workflow.prepare()
                metadata = (project / "_task-metadata/f01-files.qmd").read_text()
                for name in sources:
                    self.assertIn(f"`{name}`", metadata)
                    self.assertNotIn(f"](repo-files/{name}", metadata)
                manifest = json.loads((project / "repo-files/manifest.json").read_text())
                self.assertEqual([entry["path"] for entry in manifest], ["publication/estimates.csv"])
                self.assertEqual((project / "repo-files/publication/estimates.csv").read_bytes(), csv.read_bytes())
                self.assertFalse(list((root / "docs/publication/repo-files").iterdir()))
                for name in ("history.Rmd", "task-f01.qmd", "task-f01.qmd.txt"):
                    (project / "task-f01.qmd").write_text(f"[Source](repo-files/publication/{name})")
                    with self.assertRaisesRegex(ValueError, "not published as downloads"):
                        workflow.prepare()


if __name__ == "__main__":
    unittest.main()
