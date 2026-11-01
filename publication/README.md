# Publication

This directory is a Quarto website. Run commands here:

```sh
quarto render
quarto render -P recompute:true
quarto render task-rd02.qmd -P recompute:true
quarto render task-rd02.qmd -P refit:true
quarto render
```

Normal rendering reads saved analytical results. `recompute:true` refreshes
calculations using saved fitted models. `refit:true` also fits models; for RD02,
this includes all documented comparisons. After rendering one task, render the
site to refresh its consumers. Missing fits report the task to refit.

Each run overwrites current outputs directly. A failed run can leave partially
updated files. Preserve two copies explicitly when a comparison is needed.
There is no staging, promotion, or analytical cache to manage.

Task QMDs contain the code, scientific explanations, and review decisions.
Shared functions are defined where first used and saved for later tasks.
Explain the purpose and need before each code block. Split calculations into
coherent steps and print each result immediately after its generating code.
Recomputation is conditional inside the cell; its saved-result display runs on
every render at the same point. Tables use the shared interactive table function,
and prose uses inline R expressions. Manuscript
sections render saved CSV tables and R objects and include shared prose and figures.
`tasks.json` inventories task status, inputs, outputs, and dependencies.
Linked downloads are overwritten after rendering.

The project uses R 4.6.2 and its existing renv lockfile. Quarto reads runtime
defaults from `_environment`; explicit environment variables take precedence.
Restore packages only when needed with `renv::restore()` in this project.
Before restoring, check `.libPaths()` and `requireNamespace()`; sandbox package
inventories can be misleading. The independent thesis environment is unchanged.

Explicit checks, from the repository root:

```sh
python publication/scripts/check-analysis.py
python publication/scripts/check-website.py
```

Exported thesis inputs and their deliberate refresh operation are documented in
[inputs/thesis/README.md](inputs/thesis/README.md). The export scripts remain
under `analysis/scripts/` and use the thesis R environment.
