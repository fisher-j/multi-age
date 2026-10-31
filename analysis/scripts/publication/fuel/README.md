# Publication fuel-analysis pipeline

This directory is a publication-only branch of the thesis fuel workflow.
Raw data, thesis notebooks, and thesis-era `.rda` objects are read-only inputs.
All generated analysis files are written beneath
`analysis/outputs/publication/fuel/`.

## Documentation entry points

The [fuel pipeline map](../../../../publication/pipeline-fuel.qmd) connects the
full sequence. Each task QMD owns its analytical explanations and decisions:

- [F01](../../../../publication/task-f01.qmd): component derivation, pairing,
  exploration, candidate fitting, diagnostics, and provisional leaders.
- [RD04](../../../../publication/task-rd04.qmd): prediction estimands, inference
  audit, uncertainty, figure/table generation, and retired display history.
- [RT03](../../../../publication/task-rt03.qmd): Results claims and qualifications.

On the rendered website, use Development workflow > Fuel pipeline map. Update
the relevant task record when changing code or outputs; this README owns the
operational instructions. Scientific choices and acceptance states are recorded
on the task pages.

## Execution

Run from `analysis/` so that its `renv` environment is active:

```bat
Rscript scripts/publication/fuel/01-derive-components.R
Rscript scripts/publication/fuel/02-explore-components.R
Rscript scripts/publication/fuel/03-fit-phase-components.R
Rscript scripts/publication/fuel/04-fit-woody-pct-models.R
Rscript scripts/publication/fuel/05-evaluate-and-select-models.R
Rscript scripts/publication/fuel/07-audit-rd04-inference.R
Rscript scripts/publication/fuel/08-generate-rd04-integrated-figure.R
```

Stages 01-05 derive, explore, fit, and evaluate candidates. Stage 07 verifies
inference against thesis references and independent computational routes;
stage 08 generates the current figure and canonical estimates. Both consume
the saved model record and `rd04-inference-utils.R`; stage 08 does not read
stage 07's comparison tables. Fitting produces provisional candidates, so
consult the task records before treating any leader as an accepted model.

`00-fuel-config.R` and `03-model-utils.R` support F01. The original stage 06
implementation is archived under `retired/`; its former entry point stops with
migration instructions. Do not use it to regenerate the current display.

Before installing packages or restoring `renv`, check `.libPaths()` and actual
`requireNamespace()` calls from `analysis/`; see the repository `AGENTS.md`.

## Website review

From the repository root:

```bat
render-products.bat publication --to html
python publication/scripts/check-website.py
```

The website reads saved outputs and includes the original scripts for review.
Rendering does not run the analytical pipeline or fit models. The task pages
provide complete CSV reviews and current downloads for their respective stages.
