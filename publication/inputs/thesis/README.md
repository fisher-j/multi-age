# Thesis export boundary

These committed snapshots are the only thesis-derived numerical inputs used by
the publication pipelines. A checkout can recompute publication analyses
without opening thesis model objects or activating thesis renv.

| Export | Original object | Contract |
|---|---|---|
| `fuel-measurements.rds` | `summarize_collected_data.rda`, `d$transects` | 256 rows; phase/site/treatment/corner/azimuth identifiers and required two-station forest-floor, fuel-bed, and vegetation measurements |
| `fuel-deadwood-loads.rds` | `calculate_fuel_load.rda`, `total_load` | 256 rows; identifiers and unchanged onehr/tenhr/hundhr/thoushr loads; combined forest-floor/vegetation loads excluded |
| `regeneration-measurements.rds` | `regen_visualize.rda`, `regen_pa` | 256 species/plot rows; site, treat, plot, spp, ba_ha; original treatment/species factor levels retained |
| `fuel/thesis-reference-reproduction.csv` | `fuel_modeling_round_3.rda`, `fuel_tmb` | Historical Results grid, zero-random-effect means/SEs, and model formulas |
| `regeneration/tables/`, `regeneration/inference-audit/` | `regen_visualize.rda`, `mba` | Historical joint-model integrated means/SEs/intervals, observed summaries, prediction grids, and independent inference checks |
| `regeneration/tables/rd02-thesis-results.csv` | `results_appendix_deps.rda`, `regen_all` emmeans objects | Original 16 thesis zero-effect means, SEs, and response-scale asymptotic 95% intervals, including negative lower limits; checked against the saved `mba` predictions |
| `regeneration/saved-model-reference.rds` | Same saved `mba` | Plain numeric response ordering, saved log likelihood, and formula strings; contains no fitted model |

The typed RDS datasets preserve row order, values, missingness, classes, factor
levels and contrasts. `data-dictionary.csv` inventories every dataset column,
type, factor level and missing count. Depths are cm; vegetation heights are m;
cover/litter estimates are percentages; fuel loads are Mg/ha; basal area is
mÂ²/ha. Neither export nor migration revises measurement or scientific definitions.

`manifest.json` records SHA-256 checksums of inputs, producing scripts and all
exported numerical/audit artifacts. `export-session-info.txt` records the actual
thesis replay environment. Check export integrity explicitly with `python scripts/check-analysis.py --inputs-only`.
Publication uses these exports and its current code, lockfile and canonical
outputs; no retained migration snapshots or acceptance artifacts are required.

To deliberately refresh, from `analysis/` in a Windows command prompt:

```bat
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" scripts/export-publication-inputs.R
```

This operation uses the existing thesis renv. It loads saved thesis objects into
isolated environments, replays the historical fuel predictions and RD02
inference checks, and verifies that protected thesis files have not changed.
It does not execute thesis notebooks, refit historical models, or update thesis
packages. Review and version the changed exports before publication execution.
Scientific changes to exports or historical references require separate review.
