## Rendering and environments

- Publication is a plain Quarto project. From `publication/`, use `quarto render`.
  From the repository root, the optional alias is
  `.\render-products.bat publication --to html`.
- `tasks.json` is the authoritative dependency registry. Maintain task inputs,
  outputs, dependencies, and affected consumers using its existing fields.
  Correct missing or incorrect relationships before relying on the registry;
  manuscript-consumer relationships are not yet complete. Keep generated task
  metadata consistent by editing the registry rather than generated copies.
- Refresh affected producers and downstream consumers in dependency order,
  including manuscript pages and saved narrative helpers. The registry records
  dependencies; it does not execute them. Completion requires refreshing the
  affected dependency chain, not a separate full-site render. Use a full-project
  render when its scope is appropriate.
- Choose the execution mode for each affected task. Without flags, tasks read
  saved analytical results while still running setup, helper serialization, and
  presentation code. Add `-P recompute:true` to refresh calculations from saved
  models, or `-P refit:true` to refit and recompute. Refit when fitting code or
  fitting inputs change, or when explicitly requested. Missing saved models are
  an explicit prerequisite; do not silently refit them.
- Outputs overwrite current files directly. No staging, promotion, or automatic
  comparisons. After a failure, treat affected outputs as potentially
  inconsistent. Use the committed baseline for inspection or deliberate recovery
  and complete the necessary dependency sequence before claiming success.
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
  when needed. After package dependency changes, run the existing
  `publication/scripts/verify-environment.R` in the publication environment.

## Scientific integrity

- Before changing analytical code or overwriting analytical results, ensure
  affected versioned outputs are committed. If needed, create a focused baseline
  commit, including related existing source/input changes needed to make that
  baseline intelligible. Exclude unrelated work.
- Preserve tracking boundaries: text results and figures remain versioned;
  ignored fitted models, simulations, and serialized helpers remain ignored.
  Use commits and diffs for before/after review when comparison is needed. Do
  not add numerical comparison frameworks or retain comparison snapshots.
- Preserve scientific meaning during implementation refactoring, including data
  inclusion, units, factor coding, transformations, model specifications,
  estimands, uncertainty calculations, seeds, and consequential computational
  settings.
- Methodological changes within the requested analytical scope are allowed.
  Document their scientific rationale, assumptions, and consequences in the task
  QMD. Ask when a consequential scientific decision remains unresolved or exceeds
  the task's scope. Report scientific problems found outside that scope.
- Ground manuscript claims and captions in verified calculations or saved
  outputs. Distinguish exploratory candidates, provisional choices, and accepted
  conclusions; do not invent results or imply acceptance that has not occurred.
- Retain existing scientific checks and tolerances. Do not weaken them merely
  to obtain passing results. A render using saved results does not verify changed
  calculations inside recomputation branches.

## Publication QMD prose and code

- Publication R code belongs in task QMDs, including shared helpers and
  presentation calculations. Task QMDs are authoritative for helper implementations.
  Define helpers where first used and save them for later tasks. When a helper
  changes, regenerate its serialized copy through the defining task before
  refreshing consumers; render or recompute consumers according to its effect.
  F01 regenerates its shared helper bundle without analytical recomputation.
- Write analytical task explanations as an account of the analysis as it
  develops. Use clear, active, slightly conversational prose; "we" and "we'll"
  are appropriate. Explain the scientific reasoning without narrating routine
  programming actions. Manuscript sections, Results text, and captions retain
  their publication tone.
- Open a task or substantial analytical part with the question being addressed,
  why it matters, and how the present work helps answer it. Provide broader
  publication context only where it helps the reader understand the current work.
- Before every code block, explain its purpose and why that step is needed.
  A brief reason is sufficient for setup and helper blocks. If a block does not
  merit its own explanation, combine it with related code.
  Describe consequential choices, assumptions, or limitations where they arise.
  Comments inside code supplement this explanation.
- Split code at meaningful analytical boundaries: when the question, operation,
  or result needs its own explanation. Keep each block focused on a coherent
  step. Avoid both long blocks spanning several reasoning steps and tiny blocks
  that fragment one calculation. Use reader comprehension, rather than a fixed
  line count, to choose boundaries.
- Display each table, figure, or other analytical result immediately after its
  generating code, including results also saved to disk. Keep recomputation
  conditional inside the cell and display the current saved result at the same
  point when rendering saved results. Helper and setup blocks need an
  explanation, without an artificial result.
- After results, explain what they show and why they matter when interpretation
  is needed. Distinguish observations, model estimates, provisional choices, and
  accepted conclusions.
- Close a task or substantial analytical part with a few sentences or a short
  paragraph explaining what was accomplished or learned, why it is useful, and
  any material limitations or unresolved interpretation. Mention downstream work
  only enough to establish relevance. Avoid repeating procedural steps, artifact
  inventories, decision tables, or speculative future plans as a substitute for
  this summary.
- Use only the detail needed to guide the reader. Do not impose word counts,
  compulsory summary headings, or repetitive prose before and after every block.

## Development philosophy

This project prioritizes simplicity, maintainability, and rapid iteration.

- Implement the simplest correct solution that satisfies the stated requirements.
- Avoid speculative abstractions, general-purpose frameworks, configuration
  layers, and infrastructure for hypothetical future needs.
- Prefer straightforward functions and existing project patterns over new classes
  or architectural layers. When multiple approaches work, prefer fewer moving
  parts and lower future maintenance cost.
- Do not introduce new dependencies, tooling, scripts, or documentation files
  unless they provide a clear current benefit.
- Prefer refactoring existing code over adding parallel implementations. Treat
  reducing complexity and deleting obsolete code as positive outcomes; do not
  preserve unused APIs, wrappers, fallbacks, or legacy behavior as a precaution.
- For internal, nonproduction interfaces, breaking changes are acceptable when
  they simplify the design. Update affected callers and tests rather than adding
  compatibility wrappers. Preserve data integrity, security, and explicitly
  identified production contracts.

## Testing and verification

- Match verification to the scope and risk of the change. For instructions or
  prose, review the diff and render affected pages when rendered content changes.
  Prose alone does not require analytical recomputation or refitting; AGENTS.md
  edits alone do not require publication rendering.
- For helper or presentation changes, regenerate affected helpers and refresh
  registered consumers. For analytical or fitting changes, checkpoint first,
  refresh affected dependencies using appropriate execution modes, inspect
  existing scientific checks, and run relevant validators.
- Review important figures after significant analytical changes or refits, and
  inspect figures when requested work changes their visual presentation. Avoid
  routine image review for unrelated prose, setup, or minor code edits.
- For shared workflow or site configuration changes, run relevant existing
  regression tests and refresh affected pages. Use a full render when broadly
  applicable, and broader verification before release or for shared interfaces.
- From the repository root, use
  `python publication/scripts/check-analysis.py` for declared outputs, CSV
  structure, and basic numerical validity. Use its task/domain options for
  relevant output checks. Export integrity requires `--inputs-only`; add
  `--require-models` when checking availability of registered saved fits.
- `python publication/scripts/check-website.py` checks the rendered site,
  including links, downloads, and selected displayed tables. Neither validator
  establishes scientific correctness or freshness of calculations.
- Add tests for meaningful behavior, important edge cases, and previously
  observed bugs.
- Prefer behavioral tests over tests coupled to implementation details.
- Avoid duplicative tests and exhaustive coverage of trivial helpers.
- During development, run the narrowest relevant tests and checks.
- Do not add new test infrastructure without a demonstrated need.
- Report verification performed, omitted checks, and blockers. Retry expensive
  operations only after identifying a reason another attempt should succeed.

## Scope and completion

- Keep changes focused. Do not expand the task into unrelated cleanup or
  infrastructure work. Report unrelated warnings, failures, and environment
  issues rather than repairing them unless they prevent verification of the task.
- Do not add optional features or safeguards beyond the stated requirements
  without a concrete justification.
- When a proposed abstraction or new infrastructure adds significant complexity,
  briefly explain the trade-off before implementing it.
- Stop once the requested behavior is implemented and appropriately verified.
