"""Current-output and export-integrity validation regression tests."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("checks", Path(__file__).with_name("check-analysis.py"))
checks = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checks)


class PublicationValidationTests(unittest.TestCase):
    def project(self, root, text="estimate,se,lcl,ucl\n1,0.1,0.8,1.2\n"):
        path = root / "outputs/fuel/tables/estimates.csv"
        path.parent.mkdir(parents=True)
        path.write_text(text, encoding="utf-8")
        (root / "tasks.json").write_text(json.dumps([dict(
            outputs=["publication/outputs/fuel/tables/estimates.csv"])]), encoding="utf-8")
        return path

    def test_saved_outputs_do_not_require_historical_snapshots_or_model_caches(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            self.project(root)
            self.assertFalse(checks.validate_outputs(root, root / "outputs", ["fuel"]))

    def test_missing_declared_output_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            self.project(root).unlink()
            self.assertTrue(checks.validate_outputs(root, root / "outputs", ["fuel"]))

    def test_corrupt_intervals_and_nonfinite_values_are_rejected(self):
        for row in ("1,0.1,2,3", "NaN,0.1,0,2", "1,inf,0,2", "1,-0.1,0,2", "bad,0.1,0,2", "1,0.1,0"):
            with self.subTest(row=row), tempfile.TemporaryDirectory() as temp:
                root = Path(temp)
                self.project(root, "estimate,se,lcl,ucl\n" + row + "\n")
                self.assertTrue(checks.validate_outputs(root, root / "outputs", ["fuel"]))

    def test_recomputation_requires_registered_fitted_models(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            self.project(root, "model_id,fit_status\nmodel_a,fit\n")
            self.assertFalse(checks.validate_outputs(root, root / "outputs", ["fuel"]))
            self.assertTrue(checks.validate_outputs(root, root / "outputs", ["fuel"], require_models=True))

    def test_selected_regeneration_requires_its_fitted_models(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            table = root / "outputs/regeneration/selected-species-models/tables/rd02-selected-species-fit-register.csv"
            table.parent.mkdir(parents=True)
            table.write_text("model_id,convergence\nrw_site_macro,0\n", encoding="utf-8")
            (root / "tasks.json").write_text("[]", encoding="utf-8")
            self.assertTrue(checks.validate_outputs(root, root / "outputs", ["regeneration"], require_models=True))
            model = table.parent.parent / "models/rw_site_macro.rds"
            model.parent.mkdir()
            model.write_bytes(b"saved fit")
            self.assertFalse(checks.validate_outputs(root, root / "outputs", ["regeneration"], require_models=True))

    def test_task_checks_need_only_its_own_outputs(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            self.project(root)
            (root / "tasks.json").write_text(json.dumps([
                dict(id="F01", outputs=["publication/outputs/fuel/tables/estimates.csv"],
                     inputs=["publication/outputs/fuel/upstream.csv"]),
                dict(id="RD04", outputs=["publication/outputs/fuel/rd04.csv"]),
            ]), encoding="utf-8")
            self.assertFalse(checks.validate_outputs(root, root / "outputs", ["fuel"], task_id="F01"))
            self.assertTrue(checks.validate_outputs(root, root / "outputs", ["fuel"], task_id="RD04"))
            self.assertTrue(checks.validate_outputs(root, root / "outputs", ["fuel"]))

    def test_modified_export_is_rejected(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            exports = root / "inputs/thesis"
            exports.mkdir(parents=True)
            data = exports / "data.csv"
            data.write_bytes(b"value\n1\n")
            (exports / "manifest.json").write_text(json.dumps(dict(
                exports={"data.csv": checks.sha(data)}, inputs={})), encoding="utf-8")
            self.assertFalse(checks.validate_exports(root))
            data.write_bytes(b"value\n2\n")
            self.assertTrue(checks.validate_exports(root))
            data.unlink()
            self.assertTrue(checks.validate_exports(root))



if __name__ == "__main__":
    unittest.main()
