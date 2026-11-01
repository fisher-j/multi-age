# Redwood multi-age experiment

This is an analysis of the 10-year remeasure of the redwood multi-age experiment. It was
started by Dr. Pascal Berrill, professor of silviculture at Cal-Poly Humboldt. The
experiment is located in the Jackson Demonstration State Forest in Mendocino County,
California. The multi-age experiment explores the regeneration response of several
species following different harvesting techniques including group selection, aggregated
retention, and high/low dispersed retention. The 10-year remeasure data includes surface
fuel characterization.

Rendered products are published at
[fisher-j.github.io/multi-age](https://fisher-j.github.io/multi-age).

## Products and rendering

The repository contains related but independent scholarly products. The legacy
products remain in `analysis/` and retain their existing Quarto profiles so
that their established rendering behavior is preserved. The developing
publication is a standalone Quarto project in `publication/`. Its execution
and maintenance instructions live in [publication/README.md](publication/README.md).

`product-registry.yml` records each product's source location, render command,
and output location. From `publication/`, use `quarto render`, adding `-P recompute:true` for
calculations from saved models or `-P refit:true` for explicit model fitting.
On Windows, `render-products.bat <product> [Quarto render options]` is an
optional alias and remains the entry point for legacy products. Use
`render-products.bat` without an argument to see the supported product names.
For focused legacy checks, render only the needed format; for example,
`render-products.bat thesis --to pdf` avoids building the thesis HTML and DOCX
products during a PDF iteration.

`render-legacy-regression.bat <product> [directory]` creates a disposable copy
of the current Git revision and renders a legacy product there. Use it when a
full regression check could otherwise rewrite generated legacy objects in the
active worktree. The copy is retained for inspection and can be removed after
review.

Publication-specific analyses and generated objects belong under
the publication outputs directory, with analytical R code in task QMDs. Do not
move or overwrite the thesis-era model objects and rendered artifacts while the
publication is in development.
