# RD02 regeneration display

## Selected analysis and manuscript outputs

The selected manuscript analysis fits all four species categories separately:
`ba_ha ~ treat + (1 | site/treat)`, `ziGamma(log)`, zero `~1`, dispersion `~1`.
Run or source `05-fit-selected-species-models.R` to reproduce it. Each category
has 64 observations, four site groups, and 16 nested macro-plot groups.
The plot-level random effect is omitted. Selection follows the user's
biological modeling decision rather than the minimum-AIC alternative.

Current outputs are under `outputs/regeneration/selected-species-models/`:
16 integrated means and full-covariance intervals, four fitted models, fit and
variance registers, DHARMa plots/tests, optimizer checks, and provenance. All
four models are checked against BFGS; all 16 means are verified using model
predictions, numerical gradients, and quadrature. RW/TO means reproduce script
03's species fits. The script protects the thesis input and historical joint
reference outputs with checksums.

The current Results figure is `publication/assets/rd02-selected-species-basal-area.png`.
`publication/sections/rd02-species-estimates-table.qmd` reads the selected
estimate CSV directly for the all-category supplement table. Review downloads
are copied to `publication/assets/rd02-selected/`. Scripts 01–04 preserve the
reference/sensitivity record and do not overwrite this selected figure.

## Historical joint-model reference

From the repository root, run `run-publication.bat regeneration`. The runner
uses publication's independent R and renv and executes 01, 02, 03, 05, then 04.
Individual scripts can also be run from `publication/` with that environment:

```sh
Rscript scripts/regeneration/01-generate-rd02-display.R
```

The script can also be sourced from a publication R session. It reads exported
historical estimates and audit tables, then generates the reference display.
The original `mba` replay, prediction, gradient, and quadrature checks execute
in `analysis/scripts/export-publication-inputs.R` under thesis R and renv.
Publication never loads `analysis/regen_visualize.rda`. The selected model is `m5` from
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

Outputs in `publication/outputs/regeneration/`:

- `figures/rd02-regeneration-basal-area.png` and `.pdf`: shared-scale lead display.
- `tables/rd02-basal-area-model-estimates.csv`: all 16 species/treatment means.
- `tables/rd02-observed-basal-area-summary.csv`: observed counts, zeros, and means.
- `inference-audit/`: four-site grid and zero-effect/integrated comparison.
- `manifests/`: input/script checksums, model specification, review status, session.

The PNG is also copied to `publication/assets/`. The shared caption/display
source is `publication/sections/rd02-regeneration-display.qmd`.
Linked review downloads are bundled in `publication/assets/rd02/` so that
the rendered workflow can serve the same estimates, audit, PDF, and source.

The thesis export operation checks all 16 historical cells against `predict()` and `marginaleffects` for the
zero-effect reference, and against `marginaleffects::hypotheses()` for integrated
means and SEs. Independent numerical gradients and numerical quadrature verify
the analytic derivatives and integration factor. Export checksums are checked before publication execution and again at completion. No significance arrows, inherited p-values, or claims
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

Outputs are in `outputs/regeneration/random-effects-comparison/`:
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

## Separate RW and TO models with site and macro-plot effects

Run or source `03-compare-rd02-separate-species.R`. Each focal species uses
`ba_ha ~ treat + (1 | site/treat)`, with `ziGamma(log)`, zero `~1`, and
dispersion `~1`. There are 64 observations, 4 sites, 16 macro plots, and 64
vegetation plots per species. Each fit estimates its own variance components.
Within one species, site-by-species is simply site. The plot-level random
intercept is omitted; each species has one observation per vegetation plot.

For a valid AIC comparison, all three joint alternatives are refitted on the
same 128 RW/TO rows. The separate pair's log likelihoods and parameter counts
are summed under species independence. Neither individual-species AICs nor
the paired AIC are compared with the earlier 256-row, four-species AICs.
Cross-species covariance is not estimated by the independent fits.

Outputs are in `outputs/regeneration/separate-species-comparison/`:
fit and variance registers, same-data AIC comparison, 1,000-simulation DHARMa
checks and treatment plots, full-covariance integrated means and intervals,
BFGS/start sensitivity checks, provenance, session, and a comparison figure.
All 32 mean cells are checked using `predict()`, numerical gradients, and
quadrature. Original inputs and lead figure/estimates are checksum protected.
Review downloads are copied to `publication/assets/rd02-species/`, and the
workflow includes `publication/sections/rd02-separate-species-review.qmd`.
The script does not replace the lead model or generate significance arrows.

## Overall sensitivity figures and tables

After scripts 02, 03, and 05, run or source `04-assemble-rd02-sensitivity.R`.
It reads their generated CSVs without refitting models or recalculating
inference. The overall collection includes the three four-species joint fits,
three RW/TO joint refits, the separate site/macro-plot RW/TO pair, and the
selected four-category separate models.

`outputs/regeneration/overall-sensitivity/` contains a four-panel
figure (fitting-data scopes in rows, species in columns), an eight-configuration
fit table, all 28 variance-component rows, 48 row-level diagnostic tests, and
96 complete mean/interval cells (64 focal-species cells in the figure).
The separate species share a figure label; `component_model_id` preserves
their individual fits. AIC differences are calculated within each data scope,
never across the 256-row and 128-row groups.

The assembler checks that every source estimate and interval limit is copied
exactly, all configuration/treatment/species cells are present once, AIC
matches its log likelihood and parameter count, and source hashes are
unchanged. It copies review artifacts to `publication/assets/rd02-sensitivity/`.
The overall and focused species figures remain in the workflow with one fit
comparison table. Full estimates, variance components, and diagnostics are
downloads; duplicate rendered tables are omitted. Review prose summarizes
treatment patterns, notable variance/interval differences, and diagnostic trends.
