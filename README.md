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
publication is a standalone Quarto project in `publication/`.

`product-registry.yml` records each product's source location, render command,
and output location. On Windows, `render-products.bat <product>` provides a
common entry point for rendering one product. Use `render-products.bat` without
an argument to see the supported product names.

Publication-specific analyses and generated objects belong under
`analysis/outputs/publication/` and `analysis/scripts/publication/`. Do not
move or overwrite the thesis-era model objects and rendered artifacts while the
publication is in development.
