# Analysis outputs

This directory separates new generated analytical products from the legacy
objects stored directly in `analysis/`. The legacy objects are retained in their
current paths because the completed thesis, lab book, and slides depend on them.

- `thesis/` is intentionally not introduced as a migration target. The tagged
  thesis baseline remains reproducible from its existing paths.
- `publication/` is reserved for new publication-specific derived data, fitted
  objects, figures, and tables. New work must not overwrite the legacy objects.
