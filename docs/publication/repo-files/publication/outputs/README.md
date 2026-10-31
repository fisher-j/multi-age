# Publication outputs

Publication-specific generated artifacts are organized by analysis:

- `fuel/`: derived component data, candidate-model records, diagnostics,
  inference audits, and the RD04 inventory figure and estimates.
- `regeneration/`: the historical joint-model reference, covariance-structure
  and separate-species comparisons, selected species-category models, and
  assembled RD02 sensitivity displays.

Reproduction instructions live in `publication/scripts/fuel/README.md`
and `publication/scripts/regeneration/README.md`, relative to the
repository root. Use `run-publication.bat all` from the repository root. It selects publication
R 4.6.2 and its own renv, consumes committed thesis exports, and stages results
for current-output validation before replacing canonical outputs.
Fitted models and simulation objects are local ignored `.rds` files; tables,
figures, diagnostics, and provenance are versioned. Publication rendering reads
the saved outputs without refitting models.

These pipelines preserve the thesis-era inputs and write publication products
here and to the publication website's asset directories.
