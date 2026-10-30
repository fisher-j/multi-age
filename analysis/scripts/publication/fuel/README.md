# Publication fuel-analysis pipeline

This directory is a publication-only branch of the thesis fuel workflow. It
must not alter raw data, the thesis notebooks, or thesis-era `.rda` objects.

Run the derivation from `analysis/`:

```bat
Rscript scripts/publication/fuel/01-derive-components.R
Rscript scripts/publication/fuel/02-explore-components.R
Rscript scripts/publication/fuel/03-fit-phase-components.R
Rscript scripts/publication/fuel/04-fit-woody-pct-models.R
Rscript scripts/publication/fuel/05-evaluate-and-select-models.R
Rscript scripts/publication/fuel/08-generate-rd04-integrated-figure.R
```

`01-derive-components.R` reads `summarize_collected_data.rda` as a read-only
input. It carries the unchanged 1-, 10-, 100-, and 1,000-hour components from
the read-only thesis `calculate_fuel_load.rda` object, but does not use that
object's combined duff/litter or vegetation columns. It writes only beneath
`outputs/publication/fuel/`.

Current analytical decisions:

- Pre-PCT litter and duff are separate, model-derived forest-floor components.
  The script uses Finney and Martin (1993), model 3, with the recorded total
  forest-floor depth and estimated litter percentage. A negative value from a
  shallow extrapolation is bounded at zero and retained in the station-level
  audit output.
- Post-PCT duff and litter loads are deliberately not calculated or reported:
  slash and suspended green foliage make the forest-floor boundary ambiguous.
- Woody and herbaceous vegetation remain separate in both phases.
- The primary PCT contrast is matched-transect pre-PCT minus post-PCT woody
  standing vegetation. It is an inferred potential surface-fuel contribution,
  not a direct slash-load measurement.
- Fuel-bed depth is retained for a later protocol-comparability review and is
  not yet interpreted as slash depth.

No model family, dispersion structure, zero process, or random-effects
selection is made by the derivation script. `02-explore-components.R` fixes the
random-intercept structure dictated by the design: site, site-by-treatment
block, and corner nested within block. It reports zeros and spread by component
and proposes zero-inflation and dispersion candidates without fitting models.

The fitting scripts preserve every attempted candidate in a model registry with
its formula, family, zero and dispersion formulas, warnings, AIC, and DHARMa
tests. `05-evaluate-and-select-models.R` identifies only provisional AIC
leaders; final selection requires documented diagnostic and scientific review.

### R-D04 inference audit

The active figure generator is `08-generate-rd04-integrated-figure.R`. All
panels report equally site-weighted expected loads integrated over all fitted
random effects; woody change uses the joint repeated-phase model and its
pre/post covariance. Load intervals use the log-scale delta method and signed
change intervals use the response scale, with full parameter covariance in
both cases. Shared functions are in `rd04-inference-utils.R`.

Run `Rscript scripts/publication/fuel/07-audit-rd04-inference.R` from `analysis/`
to reproduce the thesis fuel Results grids, compare the original R-D04
zero-random-effect predictions with `marginaleffects`, and demonstrate
random-effect-integrated means with both nonlinear `marginaleffects`
inference and independently derived custom delta-method gradients. Results
and session information are written to `outputs/publication/fuel/inference-audit/`.
The original generator is archived under `retired/` for historical comparison;
its former entry point stops with migration instructions. Saved models and
thesis files are preserved. The R-D04 workflow card records the implemented
mean-load interpretation, both computational routes, and Methods wording.
