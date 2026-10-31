"""Regressions for native Quarto task selection, artifact routing and promotion."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from contextlib import redirect_stdout
from io import StringIO
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("hooks", Path(__file__).with_name("render-hooks.py"))
hooks = importlib.util.module_from_spec(spec)
spec.loader.exec_module(hooks)


class TaskExecutionTests(unittest.TestCase):
    def tasks(self):
        return [dict(id="F01", recomputable=True, outputs=["publication/outputs/fuel/data.csv"],
                     private_outputs=["publication/outputs/fuel/models/*.rds"]),
                dict(id="RD04", recomputable=True, outputs=["publication/outputs/fuel/estimates.csv"],
                     depends_on=["F01"])]

    def test_all_four_selections_route_by_producer(self):
        with tempfile.TemporaryDirectory() as directory:
            project = Path(directory)
            run = project / ".runs/test"
            for selected in ([], ["F01"], ["RD04"], ["F01", "RD04"]):
                for task, name in (("F01", "data.csv"), ("RD04", "estimates.csv")):
                    path = hooks.resolve(f"outputs/fuel/{name}", self.tasks(), selected, run, project)
                    self.assertEqual(path, (run if task in selected else project) / f"outputs/fuel/{name}")
                model = hooks.resolve("outputs/fuel/models/a.rds", self.tasks(), selected, run, project)
                self.assertEqual(model, (run if "F01" in selected else project) / "outputs/fuel/models/a.rds")

    def test_selected_missing_artifact_never_falls_back(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            saved = root / "outputs/fuel/data.csv"
            saved.parent.mkdir(parents=True)
            saved.write_text("old")
            result = hooks.resolve("outputs/fuel/data.csv", self.tasks(), ["F01"], root / ".runs/test", root)
            self.assertFalse(result.exists())

    def selection_fixture(self, project, selection):
        run = project / ".runs/test"
        hooks.write(project / ".runs/active-render.json", dict(mode="recompute", run=str(run)))
        hooks.write(project / "tasks.json", self.tasks())
        hooks.write(run / "execution.json", dict(status="awaiting_selection", selected=[], completed=[],
            documents=["task-f01.qmd", "task-rd04.qmd"]))
        hooks.write(run / "selection.json", selection)
        return run

    def test_unknown_selection_and_unrendered_selection_rejected(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            run = self.selection_fixture(root, ["UNKNOWN"])
            with self.assertRaisesRegex(ValueError, "Unsupported"):
                hooks.select()
            hooks.write(run / "selection.json", ["F01"])
            hooks.write(run / "execution.json", dict(documents=["task-rd04.qmd"]))
            with self.assertRaisesRegex(ValueError, "not a render target"):
                hooks.select()

    def test_missing_models_reject_rd04_only_before_execution(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            self.selection_fixture(root, ["RD04"])
            registry = root / "outputs/fuel/manifests/model-comparison.csv"
            registry.parent.mkdir(parents=True)
            registry.write_text("model_id,fit_status\na,fit\n")
            with self.assertRaisesRegex(ValueError, "needs saved F01 model"):
                hooks.select()

    def test_selected_dependency_must_complete_first(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            run = self.selection_fixture(root, ["F01", "RD04"])
            hooks.write(run / "execution.json", dict(selected=["F01", "RD04"], completed=[]))
            with self.assertRaisesRegex(ValueError, "has not completed"):
                hooks.begin("RD04")

    def test_failure_prevents_promotion(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            run = self.selection_fixture(root, ["F01", "RD04"])
            hooks.write(run / "execution.json", dict(status="running", selected=["F01", "RD04"], completed=["F01"]))
            with patch.object(hooks, "promote") as promote:
                with self.assertRaisesRegex(ValueError, "did not all complete"):
                    hooks.post()
                promote.assert_not_called()

    def test_failed_render_records_failure_without_changing_completed_tasks(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            run = self.selection_fixture(root, ["F01", "RD04"])
            hooks.write(run / "execution.json", dict(status="running", completed=["F01"], active_task="RD04"))
            hooks.failed()
            state = hooks.read(run / "execution.json")
            self.assertEqual(state["status"], "render_failed")
            self.assertEqual(state["completed"], ["F01"])

    def test_promotion_rolls_back_on_replacement_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            run = root / ".runs/test"
            run.mkdir(parents=True)
            first, second = root / "a.csv", root / "b.csv"
            first.write_text("old-a")
            second.write_text("old-b")
            a, b = run / "a.csv", run / "b.csv"
            a.write_text("new-a")
            b.write_text("new-b")
            original = Path.replace
            def replace(path, target):
                if target == second:
                    raise OSError("injected replacement failure")
                return original(path, target)
            with patch.object(Path, "replace", replace):
                with self.assertRaises(OSError):
                    hooks.promote([(a, first), (b, second)], run, root)
            self.assertEqual(first.read_text(), "old-a")
            self.assertEqual(second.read_text(), "old-b")

    def test_fingerprints_follow_selected_upstream(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            run = root / ".runs/test"
            for base, value in ((root, "old"), (run, "new")):
                path = base / "outputs/fuel/data.csv"
                path.parent.mkdir(parents=True)
                path.write_text(value)
            task = dict(id="RD04", inputs=["publication/outputs/fuel/data.csv"])
            saved = hooks.fingerprints(task, self.tasks(), [], run)
            active = hooks.fingerprints(task, self.tasks(), ["F01"], run)
            self.assertNotEqual(saved, active)

    def test_review_reports_changed_upstream_inputs(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(hooks, "PROJECT", Path(directory)):
            root = Path(directory)
            tasks = self.tasks()
            tasks[1].update(inputs=["publication/outputs/fuel/data.csv"],
                            execution_manifest="publication/outputs/fuel/rd04-execution.json")
            hooks.write(root / "tasks.json", tasks)
            hooks.write(root / ".runs/active-render.json", dict(mode="review"))
            data = root / "outputs/fuel/data.csv"
            data.parent.mkdir(parents=True)
            data.write_text("old")
            hooks.write(root / "outputs/fuel/rd04-execution.json",
                        dict(input_sha256={"outputs/fuel/data.csv": hooks.sha(data)}))
            data.write_text("changed")
            output = StringIO()
            with redirect_stdout(output):
                hooks.review("RD04")
            self.assertIn("Upstream inputs have changed", output.getvalue())
            self.assertIn("no recalculation", output.getvalue())


if __name__ == "__main__":
    unittest.main()
