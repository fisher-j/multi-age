"""Repository download preparation regressions."""
import importlib.util
import json
import os
import subprocess
import re
import shutil
from pathlib import Path
import tempfile
import unittest
from contextlib import redirect_stdout
from io import StringIO
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("workflow", Path(__file__).with_name("prepare-workflow.py"))
workflow = importlib.util.module_from_spec(spec)
spec.loader.exec_module(workflow)

spec = importlib.util.spec_from_file_location("website", Path(__file__).with_name("check-website.py"))
website = importlib.util.module_from_spec(spec)
spec.loader.exec_module(website)


class WorkflowTests(unittest.TestCase):
    def test_rd02_figures_stay_visible_and_supporting_tables_start_folded(self):
        with tempfile.TemporaryDirectory() as directory:
            page = Path(directory) / "task.html"
            markup = ('<img src="outputs/rd02-regeneration-basal-area.png">'
                      '<details id="rd02-historical-details"><summary>History</summary></details>'
                      '<img src="outputs/rd02-overall-sensitivity.png">'
                      '<details id="rd02-comparison-details"><summary>Fits</summary></details>')
            page.write_text(markup, encoding="utf-8")
            self.assertEqual(website.rd02_review_errors(website.Document(page)), [])
            page.write_text(markup.replace('id="rd02-comparison-details"',
                                           'id="rd02-comparison-details" open'), encoding="utf-8")
            self.assertTrue(website.rd02_review_errors(website.Document(page)))
            page.write_text('<details>' + markup + '</details>', encoding="utf-8")
            self.assertTrue(website.rd02_review_errors(website.Document(page)))

    def test_csv_formula_tildes_are_not_rendered_as_markdown_subscripts(self):
        with tempfile.TemporaryDirectory() as directory:
            page = Path(directory) / "task.html"
            for formula, broken in (("load ~ treatment + (1 | site)", False),
                                    ("load <sub> treatment + (1 | site)</sub>", True)):
                with self.subTest(formula=formula):
                    page.write_text('<details data-review-source="fits.csv"><table><tr><td>' +
                                    formula + '</td></tr></table></details>', encoding="utf-8")
                    self.assertEqual(bool(website.Document(page).formula_errors), broken)

    def test_rd02_fit_table_checks_current_candidate_count(self):
        with tempfile.TemporaryDirectory() as directory:
            page = Path(directory) / "task.html"
            markup = ('<img src="outputs/rd02-regeneration-basal-area.png">'
                      '<details id="rd02-historical-details"><summary>History</summary></details>'
                      '<img src="outputs/rd02-overall-sensitivity.png">'
                      '<details id="rd02-comparison-details"><summary>Fits</summary></details>')
            for candidates in (8, 10):
                with self.subTest(candidates=candidates):
                    rows = [["Model"]] + [[str(i)] for i in range(candidates)]
                    table = '<div id="tbl-rd02-random-effects-fit"><table>' + ''.join(
                        '<tr><td>' + row[0] + '</td></tr>' for row in rows) + '</table></div>'
                    page.write_text(markup + table, encoding="utf-8")
                    doc = website.Document(page)
                    self.assertEqual(website.rd02_review_errors(doc, rows), [])
                    self.assertTrue(website.rd02_review_errors(doc, rows[:-1]))
                    self.assertTrue(website.rd02_review_errors(doc, rows + [["missing model"]]))

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
            for bundle in (root / "docs/publication/repo-files",):
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
                # Simulate a calculation after preparation: downloads must use its new result.
                csv.write_text("estimate\n2\n")
                workflow.downloads(root / "docs/publication")
                copied = root / "docs/publication/repo-files/publication/estimates.csv"
                self.assertEqual(copied.read_bytes(), csv.read_bytes())
                self.assertFalse((copied.parents[1] / "manifest.json").exists())
                for name in ("old.qmd", "old.qmd.txt", "old.Rmd"):
                    self.assertFalse((root / "docs/publication/repo-files" / name).exists())
                for name in ("history.Rmd", "task-f01.qmd", "task-f01.qmd.txt"):
                    (project / "task-f01.qmd").write_text(f"[Source](repo-files/publication/{name})")
                    with self.assertRaisesRegex(ValueError, "not published as downloads"):
                        workflow.downloads(root / "docs/publication")

    def test_binary_model_download_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            project = Path(directory)
            (project / "task.qmd").write_text("[Fit](repo-files/model.rds)")
            with patch.object(workflow, "PROJECT", project), patch.object(workflow, "REPOSITORY", project):
                with self.assertRaisesRegex(ValueError, "binary objects"):
                    workflow.linked_downloads()

    def test_shared_helpers_and_parameters_in_fresh_r_sessions(self):
        project = Path(__file__).resolve().parents[1]
        document = (project / "task-f01.qmd").read_text(encoding="utf-8")
        setup = "\n".join(re.search(r"```\{r\}\n#\| label: " + label + r"\n(.*?)\n```", document, re.S)[1]
                          for label in ("f01-setup", "f01-parameters", "f01-table-functions",
                                        "f01-csv-review-function", "f01-save-helpers"))
        rscript = shutil.which("Rscript")
        self.assertIsNotNone(rscript, "Rscript is needed for the task behavior check")
        def run(code):
            result = subprocess.run([rscript, "-"], input=code, text=True, cwd=project,
                                    capture_output=True, encoding="utf-8")
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        with tempfile.TemporaryDirectory() as directory:
            path = json.dumps(Path(directory).as_posix())
            run("setwd(" + path + ")\nparams <- list(recompute=FALSE, refit=FALSE)\n" + setup +
                "\nstopifnot(!recompute)\nparams <- list(recompute=FALSE, refit=TRUE)\n" + setup +
                "\nstopifnot(recompute)\n")
            run("setwd(" + path + ")\n" +
                'helpers <- readRDS("outputs/helpers/task-helpers.rds")\n' +
                'stopifnot(all(vapply(helpers, is.function, logical(1))))\n' +
                'list2env(helpers, envir=.GlobalEnv)\n' +
                'saveRDS(list(coefficient=2), "fit.rds")\n' +
                'stopifnot(read_saved_model("fit.rds", "RD02")$coefficient == 2)\n' +
                'error <- tryCatch(read_saved_model("absent.rds", "RD02"), error=conditionMessage)\n' +
                'stopifnot(grepl("quarto render task-rd02.qmd -P refit:true", error, fixed=TRUE))\n')

    def test_rd02_integrates_site_variation_in_zero_probability(self):
        project = Path(__file__).resolve().parents[1]
        document = (project / "task-rd02.qmd").read_text(encoding="utf-8")
        helper = re.search(r"```\{r\}\n#\| label: rd02-integrated-means\n(.*?)\n```",
                           document, re.S)[1]
        code = "library(glmmTMB)\nlibrary(dplyr)\n" + helper + r'''
set.seed(217)
d <- expand.grid(plot=1:30, treat=factor(c("gs", "ld", "ha", "hd")), site=factor(1:4))
site_positive <- c(-.6, .5, -.2, .3)
site_zero <- c(-1.5, -.5, .5, 1.5)
macro <- rnorm(16, sd=.3)
mu <- exp(1 + c(0, -.2, -.4, -.6)[d$treat] + site_positive[d$site] +
          macro[as.integer(interaction(d$site, d$treat))])
d$ba_ha <- rgamma(nrow(d), shape=3, scale=mu/3)
d$ba_ha[runif(nrow(d)) < plogis(-.4 + site_zero[d$site])] <- 0
m <- glmmTMB(ba_ha ~ treat + (1 | site/treat), data=d, family=ziGamma("log"),
             ziformula=~1 + (1 | site),
             control=glmmTMBControl(optCtrl=list(iter.max=10000, eval.max=10000)))
result <- integrated_means(m, "fixture", species="rw")
# Independent midpoint-normal quadrature checks the marginal probability.
z <- qnorm((seq_len(200000)-.5)/200000)
p <- mean(plogis(unname(fixef(m)$zi) + sqrt(VarCorr(m)$zi$site[1,1])*z))
conditional_variance <- sum(vapply(VarCorr(m)$cond, function(v) v[1,1], numeric(1)))
grid <- d[match(result$treat, d$treat), ]
X <- model.matrix(~treat, grid)
expected <- exp(drop(X %*% fixef(m)$cond) + conditional_variance/2)*(1-p)
stopifnot(max(abs(result$marginal_zero_probability-p)) < 1e-6,
          max(abs(result$estimate/expected-1)) < 1e-6,
          all(result$lcl < result$estimate), all(result$estimate < result$ucl),
          max(abs(result$estimate/result$estimate[1] -
                  exp(drop(X %*% fixef(m)$cond) - drop(X[1,,drop=FALSE] %*% fixef(m)$cond)))) < 1e-7)
# The constant-zero boundary remains the original analytic calculation.
baseline <- update(m, ziformula=~1)
b <- integrated_means(baseline, "baseline", species="rw")
stopifnot(max(abs(b$marginal_zero_probability-plogis(fixef(baseline)$zi))) < 1e-12)
'''
        result = subprocess.run([shutil.which("Rscript"), "-"], input=code, text=True,
                                cwd=project, capture_output=True, encoding="utf-8")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_chunk_labels_are_added_once_to_echoed_code(self):
        project = Path(__file__).resolve().parents[1]
        document = (project / "task-f01.qmd").read_text(encoding="utf-8")
        setup = re.search(r"```\{r\}\n#\| label: f01-setup\n(.*?)\n```", document, re.S)[1]
        # Register in a preceding setup cell, as on the actual task pages.
        fixture = "```{r setup, include=FALSE}\n" + setup + "\n```\n" + '''```{r first, echo=TRUE}
value <- 40 + 2
value
```
```{r hidden, echo=FALSE}
stopifnot(value == 42)
```
```{r repeated-setup, include=FALSE}
enable_chunk_labels()
```
```{r second, echo=TRUE}
stopifnot(value == 42)
```
'''
        code = ('rendered <- knitr::knit(text=' + json.dumps(fixture) + ', quiet=TRUE)\n' +
                'stopifnot(length(gregexpr("# first", rendered, fixed=TRUE)[[1]]) == 1L,\n' +
                '          length(gregexpr("# second", rendered, fixed=TRUE)[[1]]) == 1L,\n' +
                '          grepl("# first", rendered, fixed=TRUE),\n' +
                '          grepl("# second", rendered, fixed=TRUE),\n' +
                '          !grepl("# hidden", rendered, fixed=TRUE),\n' +
                '          grepl("42", rendered, fixed=TRUE))\n')
        result = subprocess.run([shutil.which("Rscript"), "-"], input=code, text=True,
                                cwd=project, capture_output=True, encoding="utf-8")
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_refit_then_recompute_uses_the_saved_model(self):
        project = Path(__file__).resolve().parents[1]
        document = (project / "task-f01.qmd").read_text(encoding="utf-8")
        def chunk(label):
            return re.search(r"```\{r\}\n#\| label: " + label + r"\n(.*?)\n```", document, re.S)[1]
        with tempfile.TemporaryDirectory() as directory:
            code = "setwd(" + json.dumps(Path(directory).as_posix()) + ")\n"
            code += "params <- list(recompute=FALSE, refit=TRUE)\n" + chunk("f01-parameters") + "\n"
            code += chunk("f01-fit-and-diagnose") + "\n"
            code += r'''library(glmmTMB)
set.seed(42)
fixture_data <- data.frame(site=factor(rep(1:8, each=8)), treatment=factor(rep(1:2, 32)))
fixture_data$load <- rgamma(64, shape=3, rate=1) * exp(rep(rnorm(8, sd=.3), each=8))
paths <- list(output="outputs/fuel", manifests="outputs/fuel/manifests")
dir.create(paths$manifests, recursive=TRUE)
fixture_family <- Gamma("log")
calculate <- function(...) fit_and_diagnose(
  model_id = "fixture",
  response = "load",
  analysis = "fixture",
  data = fixture_data,
  formula = load ~ treatment + (1 | site),
  family = fixture_family,
  ziformula = ~0,
  dispformula = ~1,
  paths = paths,
  simulation_n = 30,
  simulation_seed = 42,
  ...)
record <- calculate()
stopifnot(record$fit_status == "fit",
          record$family == "Gamma(link = log)",
          record$ziformula == "~0", record$dispformula == "~1",
          record$fixed_and_random_formula == "load ~ treatment + (1 | site)")
stopifnot(default_formula_label(~ treatment * site) == "~treatment * site",
          default_formula_label(~ `fuel load` + site) == "~`fuel load` + site",
          default_family_label(Gamma("identity")) == "Gamma(link = identity)",
          default_family_label(glmmTMB::ziGamma("log"), "ziGamma(link = log)") ==
            "ziGamma(link = log)")
long_formula <- reformulate(paste0("predictor", seq_len(100)))
label <- default_formula_label(long_formula)
stopifnot(!grepl("\n", label, fixed=TRUE),
          identical(as.list(long_formula), as.list(as.formula(label))))
write.csv(record, file.path(paths$manifests, "model-comparison.csv"), row.names=FALSE)
model_path <- "outputs/fuel/models/fixture.rds"
before <- tools::md5sum(model_path)
params$refit <- FALSE
trace("glmmTMB", where=asNamespace("glmmTMB"),
  tracer=quote(stop("Unexpected refit")), print=FALSE)
refreshed <- calculate()
stopifnot(refreshed$fit_status == "fit", identical(before, tools::md5sum(model_path)))
custom <- calculate(family_label="Custom family", ziformula_label="No zeros",
                    dispformula_label="Common dispersion")
stopifnot(custom$family == "Custom family", custom$ziformula == "No zeros",
          custom$dispformula == "Common dispersion")
# The saved call must also work after its original data variable disappears.
fit <- read_saved_model(model_path, "F01")
rm(fixture_data)
stopifnot(length(predict(fit, type="response", re.form=NA)) == 64L)
'''
            result = subprocess.run([shutil.which("Rscript"), "-"], input=code, text=True,
                                    cwd=project, capture_output=True, encoding="utf-8")
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_cells_render_tables_and_later_pages_read_current_csv(self):
        project = Path(__file__).resolve().parents[1]
        libraries = subprocess.run([shutil.which("Rscript"), "-e",
            'stopifnot(requireNamespace("knitr"), requireNamespace("rmarkdown")); cat(.libPaths(), sep=";")'],
            cwd=project, capture_output=True, encoding="utf-8")
        self.assertEqual(libraries.returncode, 0, libraries.stdout + libraries.stderr)
        # Quarto's globs cannot traverse the brace-containing Windows sandbox temp path.
        (project / ".quarto").mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=project / ".quarto") as directory:
            fixture = Path(directory)
            (fixture / "_quarto.yml").write_text(
                "project:\n  type: default\n  output-dir: _site\n"
                "  render: [producer.qmd, consumer.qmd]\n")
            (fixture / "values.csv").write_text("Species,Estimate\nOld,1\n")
            setup = ('```{r}\n#| include: false\npublication_table <- readRDS(' +
                     json.dumps((project / "outputs/helpers/task-helpers.rds").as_posix()) +
                     ')$publication_table\n```\n')
            (fixture / "producer.qmd").write_text(setup + '''```{r}
#| label: tbl-produced
#| tbl-cap: Current estimates
values <- data.frame(Species="Redwood", Estimate=12.34, Small=0.004567,
                     Large=1234.5, Unit=1.04, Zero=0, Count=1234L,
                     Formula="load ~ treatment + (1 | site)", Literal="<b>001</b>",
                     model_id="full_nested", component_model_id="rw_site_macro",
                     provisional_aic_leader="woody_phase_zigamma")
write.csv(values, "values.csv", row.names=FALSE)
publication_table(values)
```
''')
            (fixture / "consumer.qmd").write_text(setup + '''```{r}
#| label: tbl-reused
#| tbl-cap: Reused estimates
publication_table(read.csv("values.csv"))
```
''')
            result = subprocess.run([shutil.which("quarto"), "render"], cwd=fixture,
                                    capture_output=True, encoding="utf-8",
                                    env={**os.environ, "R_LIBS": libraries.stdout.strip()})
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            for page, label in (("producer", "tbl-produced"), ("consumer", "tbl-reused")):
                rendered = website.Document(fixture / ("_site/" + page + ".html"))
                self.assertEqual(rendered.result_tables[label],
                                 [["Species", "Estimate", "Small", "Large", "Unit", "Zero", "Count", "Formula", "Literal",
                                   "model_id", "component_model_id", "provisional_aic_leader"],
                                  ["Redwood", "12", "0.0046", "1200", "1.0", "0", "1234",
                                   "load ~ treatment + (1 | site)", "<b>001</b>",
                                   "full nested", "rw site macro", "woody phase zigamma"]])
                self.assertEqual(len(rendered.widgets), 1)
                self.assertFalse(rendered.widget_errors)
                self.assertEqual(rendered.widgets[0]["options"]["order"], [])
            self.assertIn("full_nested", (fixture / "values.csv").read_text())
            self.assertIn("rw_site_macro", (fixture / "values.csv").read_text())

    def test_pairing_cell_displays_results_beside_code_in_review_and_recompute(self):
        project = Path(__file__).resolve().parents[1]
        document = (project / "task-f01.qmd").read_text(encoding="utf-8")
        chunk = re.search(r"```\{r\}\n#\| label: f01-01-pair-transects\n(.*?)\n```", document, re.S)[1]
        libraries = subprocess.run([shutil.which("Rscript"), "-e",
            'stopifnot(requireNamespace("dplyr"), requireNamespace("tidyr"), requireNamespace("readr")); cat(.libPaths(), sep=";")'],
            cwd=project, capture_output=True, encoding="utf-8")
        self.assertEqual(libraries.returncode, 0, libraries.stdout + libraries.stderr)
        (project / ".quarto").mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=project / ".quarto") as directory:
            fixture = Path(directory)
            (fixture / "_quarto.yml").write_text(
                "project:\n  type: default\n  output-dir: _site\n  render: [review.qmd]\nformat: html\n")
            data = fixture / "outputs/fuel/data"
            data.mkdir(parents=True)
            expected = [
                ["matched woody pair", "1"],
                ["post-PCT woody observation", "2"],
                ["post-PCT woody observation without pre-PCT match", "1"],
                ["pre-PCT woody observation", "2"],
                ["pre-PCT woody observation without post-PCT match", "1"],
            ]
            csv_path = data / "fuel-pairing-qc.csv"
            csv_path.write_text("status,n_transects\n" + "".join(",".join(row) + "\n" for row in expected))
            setup = ('```{r}\n#| include: false\nlibrary(dplyr)\nlibrary(tidyr)\nlibrary(readr)\n' +
                     'publication_table <- readRDS(' +
                     json.dumps((project / "outputs/helpers/task-helpers.rds").as_posix()) +
                     ')$publication_table\n' + r'''if (recompute) {
  paths <- list(data = "outputs/fuel/data")
  ids <- c("site", "treatment", "corner", "azi")
  # One matched transect, one pre-only transect, and one post-only transect.
  fuel_components_transect <- data.frame(
    site = "A", treatment = "gs", corner = 1, azi = c(1, 1, 2, 3),
    phase = c("prepct", "postpct", "prepct", "postpct"),
    component = "woody_vegetation", load_mgha = c(4, 1, 2, 3))
  fuel_components_transect <- bind_rows(fuel_components_transect,
    mutate(fuel_components_transect, component = "herbaceous_vegetation"))
  forest_floor_station <- data.frame(
    site = "A", treatment = "gs", corner = 1, azi = c(1, 1, 2, 3),
    phase = c("prepct", "postpct", "prepct", "postpct"), fuel_bed_depth_cm = 5)
}
```
''')
            for recompute in (False, True):
                with self.subTest(recompute=recompute):
                    # In review mode the calculation inputs do not exist: only the saved CSV does.
                    mode = '```{r}\n#| include: false\nrecompute <- ' + str(recompute).upper() + '\n```\n'
                    (fixture / "review.qmd").write_text(mode + setup +
                        "Audit pre/post pairing before calculating the woody contrast.\n\n" +
                        "```{r}\n#| label: f01-01-pair-transects\n" + chunk + "\n```\n", encoding="utf-8")
                    result = subprocess.run([shutil.which("quarto"), "render"], cwd=fixture,
                                            capture_output=True, encoding="utf-8",
                                            env={**os.environ, "R_LIBS": libraries.stdout.strip()})
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    page = fixture / "_site/review.html"
                    html = page.read_text(encoding="utf-8")
                    rendered = website.Document(page)
                    self.assertEqual(len(rendered.widgets), 1)
                    self.assertFalse(rendered.widget_errors)
                    self.assertEqual(rendered.widgets[0]["data"],
                                     [[row[0] for row in expected], [1, 2, 1, 2, 1]])
                    # The table belongs to the producing cell, after its visible source.
                    source_end = html.index('fuel-pairing-qc.csv')
                    cell = html[html.rfind('<div class="cell">', 0, source_end):]
                    self.assertLess(cell.index('cell-code'), cell.index('cell-output'))
                    self.assertIn('fuel-pairing-qc.csv', cell[:cell.index('cell-output')])
                    self.assertEqual((data / "fuel-paired-transect.rds").exists(), recompute)


if __name__ == "__main__":
    unittest.main()
