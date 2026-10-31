# Publication outputs

Publication-specific generated artifacts are organized by analysis:

- `fuel/`: derived component data, candidate-model records, diagnostics,
  inference audits, and the RD04 inventory figure and estimates.
- `regeneration/`: the historical joint-model reference, covariance-structure
  and separate-species comparisons, selected species-category models, and
  assembled RD02 sensitivity displays.

Reproduction instructions live in `analysis/scripts/publication/fuel/README.md`
and `analysis/scripts/publication/regeneration/README.md`, relative to the
repository root. Run those scripts from `analysis/` to use its R environment.
Fitted models and simulation objects are local ignored `.rds` files; tables,
figures, diagnostics, and provenance are versioned. Publication rendering reads
the saved outputs without refitting models.

These pipelines preserve the thesis-era inputs and write publication products
here and to the publication website's asset directories.
