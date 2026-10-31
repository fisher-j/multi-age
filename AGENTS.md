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

# Documentation ownership

- Start fuel work with `publication/pipeline-fuel.qmd` for execution order and
  dependencies, then read the owning task QMD and its included review sections.
- F01 owns component derivation, exploration, candidate fitting, and provisional
  model leaders; RD04 owns prediction/inference, uncertainty, figure/table
  outputs, and retired display history; RT03 owns fuel Results claims.
- Update analyses, outputs, decisions, implications, and review states in the
  owning task record first. Consumers link to upstream decisions and document
  their own use. Keep `tasks.json` summaries and file roles aligned with that record.
- Keep the pipeline overview as a map. If a combined review becomes useful,
  assemble it with Quarto includes of step-owned sections, not copied content.
- See `publication/README.md` for metadata roles and validation requirements.
