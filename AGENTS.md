# Rendering

- From the repository root, render with
  `.\render-products.bat publication --to html`, then validate with
  `python publication/scripts/check-website.py`.
- Use the wrapper: it handles sandbox R library paths and preserves explicit
  `R_LIBS_USER` settings. For direct Quarto commands, if unset, set
  `$env:R_LIBS_USER = "$env:USERPROFILE/AppData/Local/R/win-library/%v"`
  in the same shell invocation.
- Run legacy R work from `analysis/` to activate its `renv` environment;
  `publication/` uses a separate library.
- Before installing packages, restoring `renv`, or declaring rendering blocked,
  check `.libPaths()` and actual `requireNamespace()` calls in the target project.
  Sandbox permission checks can make `renv::status()` and `installed.packages()`
  falsely report missing packages. If needed, compare readable package
  `DESCRIPTION` versions against `analysis/renv.lock`.
