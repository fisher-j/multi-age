## Rendering

- From the repository root, render with
  `.\render-products.bat publication --to html`, then validate with
  `python publication/scripts/check-website.py`.
- Use the wrapper: it handles sandbox R library paths and preserves explicit
  `R_LIBS_USER` settings. For direct Quarto commands, if unset, set
  `$env:R_LIBS_USER = "$env:USERPROFILE/AppData/Local/R/win-library/%v"`
  in the same shell invocation.
- Run legacy R work from `analysis/` to activate its `renv` environment;
  `publication/` uses its own updated R 4.6.2 and renv. Use
  `.\run-publication.bat restore`, then `fuel`, `regeneration`, `all`, or `check`
  from the repository root. Root wrappers select each R installation explicitly.
- Publication reads committed `publication/inputs/thesis/` exports and writes
  staged calculations before current-output validation and promotion. Historical
  replay is the separate `analysis/scripts/export-publication-inputs.R`
  operation with thesis R 4.3.2; do not restore or update thesis renv for
  publication work. Publication has one authoritative environment and current
  workflow; do not restore old publication replay modes or migration artifacts.
- Before installing packages, restoring `renv`, or declaring rendering blocked,
  check `.libPaths()` and actual `requireNamespace()` calls in the target project.
  Sandbox permission checks can make `renv::status()` and `installed.packages()`
  falsely report missing packages. If needed, compare readable package
  `DESCRIPTION` versions against `analysis/renv.lock`.

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
