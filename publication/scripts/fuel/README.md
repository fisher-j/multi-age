# Fuel analytical accounts

[F01](../../task-f01.qmd) owns derivation, exploration, candidate fitting,
diagnostics and provisional leaders, in calculation order.
[RD04](../../task-rd04.qmd) owns prediction/inference, uncertainty, audits,
figure/table generation and retired display history. RT03 owns Results claims.
This directory retains reusable helpers, not another execution sequence.

From the repository root, review all saved outputs:

```bat
render-products.bat publication --to html
python publication/scripts/check-website.py
```

Recompute both fuel tasks with native Quarto controls:

```bat
render-products.bat publication --profile recompute --execute-params recompute-fuel.yml --to html
```

`run-publication.bat fuel` is a compatibility alias. `all` runs fuel followed by
the unchanged regeneration workflow, validating/promoting each domain separately.
The wrapper selects publication R 4.6.2 and renv. Before installing/restoring
packages, check `.libPaths()` and actual namespace loads; see `AGENTS.md`.

Use a native parameter YAML file to select whole tasks, for example:

```yaml
recompute: [RD04]
```

Parameter paths are relative to `publication/`, where the wrapper runs Quarto.
`recompute: []` reviews everything. Only F01 and RD04 currently support
recomputation; future executable task pages opt in through task metadata.
Nonempty selections require `--profile recompute`. Quarto orders F01 before
RD04 and runs each page in its own R session. Selection never expands implicitly:
RD04 alone needs saved F01 models; F01 alone does not regenerate RD04 estimates.
Stage 08 consumes saved F01 models and inference functions, not stage 07 CSVs.

The profile writes HTML to `.runs/recompute/site`. Use native
`--output-dir .runs/<new-run-name>/site` to retain another run. Existing run
records are not overwritten. Failed runs remain available for diagnosis.
The pre-render hook checks exports/environment and prepares staging. Native
execution parameters reach document setup, so selection and prerequisites are
validated there before calculations. The post-render hook validates selected
outputs, refreshes staged downloads, then promotes only those tasks' artifacts.
Input fingerprints identify upstream changes without accepting model assumptions.

Selected producers supply staged artifacts; unselected producers supply saved
canonical artifacts. Missing selected outputs never fall back to old files.
Private RDS/model files are inventoried but are not published as downloads.
Render normally after promotion to update the canonical website without refits.
