# RD02 regeneration display

From `analysis/`, with the project R libraries available:

```sh
Rscript scripts/publication/regeneration/01-generate-rd02-display.R
```

The script can also be sourced from an R session. It reads `mba` from
`analysis/regen_visualize.rda` into an isolated environment. It does not refit
models or change thesis files. The selected model is `m5` from
`analysis/analysis/regen_visualize.qmd`: zero-inflated Gamma, conditional log
link, `ba_ha ~ treat * spp + (1 | site:spp)`, zero and dispersion formulas
`~spp`. All four species classes remain in the fit. Only redwood and tanoak
enter the main display. This is basal area per hectare, not stem density.

For species g and treatment t, the integrated expectation is
`(1 - p_zero[g]) * exp(X[t,g] beta + sigma_site:spp^2 / 2)`.
There are no random effects in the zero component. Gamma dispersion needs no
lognormal residual correction. Four site rows receive equal weights, but the
integrated expectations coincide because site is not a fixed predictor.
This is the same arithmetic-response estimand used for RD04, audited here
for the regeneration model's distinct structure.

The full `sdr$cov.fixed` covariance propagates uncertainty in conditional and
zero coefficients and the log random-effect SD, including cross-covariances.
Means use positive 95% log-scale delta-method intervals. Model-selection,
measurement/aggregation, and model-structure uncertainty are not included.
The retained site-by-species intercept does not explicitly model macro-plot
clustering, unlike the fuel hierarchy. That design question remains open for
scientific review; this script does not silently replace the thesis model.

Outputs in `analysis/outputs/publication/regeneration/`:

- `figures/rd02-regeneration-basal-area.png` and `.pdf`: shared-scale lead display.
- `tables/rd02-basal-area-model-estimates.csv`: all 16 species/treatment means.
- `tables/rd02-observed-basal-area-summary.csv`: observed counts, zeros, and means.
- `inference-audit/`: four-site grid and zero-effect/integrated comparison.
- `manifests/`: input/script checksums, model specification, review status, session.

The PNG is also copied to `publication/assets/`. The shared caption/display
source is `publication/sections/rd02-regeneration-display.qmd`.
Linked review downloads are bundled in `publication/assets/rd02/` so that
the rendered workflow can serve the same estimates, audit, PDF, and source.

Every run checks all 16 cells against `predict()` and `marginaleffects` for the
zero-effect reference, and against `marginaleffects::hypotheses()` for integrated
means and SEs. Independent numerical gradients and numerical quadrature verify
the analytic derivatives and integration factor. The input checksum is checked
again at completion. No significance arrows, inherited p-values, or claims
based on CI overlap are generated. Refitted bootstrap intervals remain possible
later modeling work.

## Full nested design comparison

Run or source `02-compare-rd02-random-effects.R` with the same R libraries.
It refits three models on the same 256 saved species/vegetation-plot rows:

- Thesis: `ba_ha ~ treat * spp + (1 | site:spp)`.
- Full design: `ba_ha ~ treat * spp + (1 | site/treat/plot)`.
- Sensitivity: full design plus `(1 | site:spp)`.

The full design expands to site, site:treatment (macro plot), and
site:treatment:plot (vegetation plot) intercepts: 4, 16, and 64 groups.
Plot/corner codes are reused and are not treated as globally unique IDs.
All candidates use zero-inflated Gamma with conditional log link, the same
`treat*spp` fixed effects, and `~spp` zero and dispersion formulas. No random
effects are added to the zero model. The sensitivity candidate distinguishes
adding design clustering from removing species-specific site variation.

Outputs are in `outputs/publication/regeneration/random-effects-comparison/`:
ML fit/AIC/BIC/convergence records, variance components, row/grouped DHARMa
tests and plots, integrated means and full-covariance delta-method intervals,
optimizer and residual-tail sensitivity checks, and a model-comparison figure.
The two primary covariance structures are not nested in one another; the
comparison does not report an ordinary likelihood-ratio test. Lower AIC alone
does not settle the scientific adequacy of a covariance structure.

The comparison checks refitted thesis likelihood against the saved fit,
prediction matrices against `predict()`, and all integrated-mean gradients
against numerical derivatives. It checks design-candidate optima using BFGS
from initial random SDs of 0.3, alongside the main nlminb fits. Near-zero
variance components are recorded rather than silently dropped. Their
delta-method intervals are approximate near the boundary; no refitted
bootstrap intervals are claimed.

DHARMa uses 1,000 unconditional simulations per model (seed 20261030), with
recalculated residuals at site, macro-plot, and vegetation-plot levels. These
checks are exploratory, not proof of independence; four sites provide little
power at the site level. The baseline tail flag is checked again with 10,000
simulations and a different seed; flagged rows from both runs are retained.
All observations remain in the fits.

The original thesis input and current lead figure/estimates are checksum
protected. Review downloads are copied to `publication/assets/rd02-comparison/`;
fitted models and simulations remain local RDS artifacts. The comparison is
recorded in `publication/sections/rd02-random-effects-review.qmd`, included
in the RD02 workflow card. It does not automatically replace the lead model.
