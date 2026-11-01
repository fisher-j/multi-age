## Rendering

- Publication is a plain Quarto project. From `publication/`, use `quarto render`.
  From the repository root, the optional alias is
  `.\render-products.bat publication --to html`.
- Normal renders use saved analytical results. Add `-P recompute:true` to
  refresh calculations from saved models, or `-P refit:true` to refit explicitly.
  A single task can be rendered with either flag; then render the site.
- Publication R code belongs in task QMDs, including shared helpers and
  presentation calculations. Define helpers where first used and save them for
  later tasks. Explain purpose and important considerations alongside the code.
- Before every publication code block, explain its purpose and why it is needed.
  Split long calculations into coherent steps. Print tables, figures, and other
  results immediately after the code that generates them, including when results
  are also saved. Review renders should display the saved result at that same
  point; keep recomputation conditional inside the cell instead of hiding output.
- Outputs overwrite current files directly. No staging, promotion, or automatic
  comparisons. A failed run may leave partially updated outputs.
- Validate with `python publication/scripts/check-analysis.py` and
  `python publication/scripts/check-website.py` from the repository root.
- Publication has its own R 4.6.2 and renv; Quarto's `_environment` supplies
  runtime defaults while preserving explicit environment settings. Legacy work
  runs from `analysis/` under thesis R 4.3.2 and its unchanged renv.
- Committed `publication/inputs/thesis/` exports supply publication inputs.
  Deliberate refresh uses `analysis/scripts/export-publication-inputs.R` in the
  thesis environment; do not restore or update thesis renv for publication work.
- Before installing packages or restoring renv, check `.libPaths()` and actual
  `requireNamespace()` calls in the target project. Sandbox permission checks
  can make `renv::status()` and `installed.packages()` falsely report missing
  packages. Compare readable package DESCRIPTION versions to its lockfile
  when needed.

## Development philosophy

This project prioritizes simplicity, maintainability, and rapid iteration.

- Implement the simplest correct solution that satisfies the stated requirements.
- Avoid speculative abstractions, general-purpose frameworks, configuration layers, and infrastructure for hypothetical future needs.
- Prefer straightforward functions and existing project patterns over new classes or architectural layers.
- Do not introduce new dependencies, tooling, scripts, or documentation files unless they provide a clear current benefit.
- Favor deleting obsolete code over preserving backward compatibility when the affected interface is not an established external contract.
- Do not preserve unused APIs, wrappers, fallbacks, or legacy behavior merely as a precaution.
- Keep changes focused. Do not expand the task into unrelated cleanup or infrastructure work.
- Prefer refactoring existing code over adding parallel implementations.

## Testing and verification

- Add tests for meaningful behavior, important edge cases, and previously observed bugs.
- Prefer behavioral tests over tests coupled to implementation details.
- Avoid duplicative tests and exhaustive coverage of trivial helpers.
- During development, run the narrowest relevant tests and checks.
- Run broader verification before release or when changes affect shared interfaces.
- Do not add new test infrastructure without a demonstrated need.
- Report what was tested and what was not.

## Refactoring

- Treat reducing complexity and removing obsolete code as positive outcomes.
- For internal, nonproduction interfaces, breaking changes are acceptable when they simplify the design.
- Update affected callers and tests rather than automatically adding compatibility wrappers.
- Preserve data integrity, security, and explicitly identified production contracts.
- When multiple approaches work, prefer the one with fewer moving parts and lower future maintenance cost.

## Scope and completion

- Do not add optional features or safeguards beyond the stated requirements without a concrete justification.
- When a proposed abstraction or new infrastructure adds significant complexity, briefly explain the trade-off before implementing it.
- Stop once the requested behavior is implemented and appropriately verified.
