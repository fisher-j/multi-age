# Publication website maintenance

The publication is an independent Quarto website rendered to `docs/publication/`.
Its top navigation contains **Development workflow** and **Manuscript draft**,
each with a dedicated sidebar. The repository's legacy products are separate.

- `workflow.qmd`: process overview, repository map, and documentation responsibilities.
- `task-f01.qmd` and `task-rd04.qmd`: executable fuel accounts, in that order.
- `tasks.json`: index metadata (statuses, targets, short summaries, dependencies,
  and file roles); detailed analytical documentation belongs in task QMDs.
- `task-list.qmd`: generated register and task summaries; do not edit directly.
- `task-*.qmd`: dedicated task proposals, detailed decisions, review artifacts,
  inference records, and acceptance criteria.
- `_task-metadata/`: generated status lines and repository-file lists for task pages.
- `manuscript.qmd`: draft overview; `draft-*.qmd`: individual section wrappers.
- `manuscript-full.qmd`: continuous manuscript assembly.
- `sections/`: shared manuscript prose, figures, tables, and detailed reviews.

Update the dedicated task record first, then revise its `tasks.json` summary
to reflect the proposal, approaches examined, analyses, findings, and review
decisions actually recorded there. Use ordinary prose and links to relevant
sections; summaries appear only in the task list. The register links to those
entries on the same page. Keep review and
acceptance states explicit. Queued tasks remain queued when another task
contributes starting material. Edit manuscript prose only in `sections/`.

From the repository root, run:

```bat
render-products.bat publication --to html
python publication/scripts/check-website.py
```

The wrapper explicitly selects publication R 4.6.2 and activates this directory's
independent `renv` project. The legacy products explicitly select thesis R 4.3.2.
`PUBLICATION_R_HOME` and `THESIS_R_HOME` can override installation locations;
neither wrapper changes the machine PATH. An explicit `R_LIBS_USER` is preserved.
Actual namespace loads and installed DESCRIPTION versions are checked against
the publication lock; broad startup package scans are disabled because sandbox
permission checks can misreport missing packages.
Before installing packages or restoring renv, check `.libPaths()` and actual
`requireNamespace()` calls in `publication/`. Use `run-publication.bat restore`
when restoration is needed; do not restore or update the thesis renv for
publication work.

## Independent calculations and selective review

Review is the default: calculation code is displayed and evidence is read from
saved outputs. Ordinary rendering never fits models and needs no model cache.
Explicitly recompute from the repository root:

```bat
run-publication.bat f01
run-publication.bat rd04
run-publication.bat fuel
```

`f01` executes the five calculation chunks in `task-f01.qmd` order. `rd04`
executes the audit and figure chunks in `task-rd04.qmd` order using saved F01
models and registers. Missing inputs or fitted models fail with an error.
`fuel` runs F01 then RD04, each in its own Quarto/R session. There is no automatic
dependency expansion: F01 alone leaves RD04 unchanged; RD04 alone never refits
F01. The equivalent single-page invocation is
`render-products.bat publication task-rd04.qmd -P recompute:true --output-dir .runs/rd04/site --to html`.
Only the two fuel task pages declare this boolean parameter; its default is false.

Each task starts with an empty `.runs/f01/outputs` or `.runs/rd04/outputs`.
Calculations and that page's evidence use this one output directory; RD04 reads
its F01 inputs directly from canonical `outputs/fuel/`. After every calculation
and display chunk succeeds, task-specific output checks run and the staged files
are copied into canonical `outputs/`. A calculation/check failure preserves the
last successful outputs. Scratch directories are reused automatically on rerun.
There are no backups or rollback for a copy failure. Recompute HTML stays in the
task's `.runs/<task>/site`; render normally afterward to refresh the website and
downloads from saved outputs.

`all` runs fuel followed by regeneration. `regeneration` retains its separate
script sequence (01, 02, 03, 05, 04), numerical checks, and staged output copy.
Run `run-publication.bat regeneration` from the repository root for that
sequence. Individual regeneration scripts can also be run or sourced from
`publication/` in its R environment, for example
`Rscript scripts/regeneration/01-generate-rd02-display.R`.
`check` validates saved outputs without requiring model caches; `--require-models`
on the Python checker additionally checks models. Existing prediction, gradient,
integration and covariance checks remain in the owning QMD calculations.

`publication/renv.lock`, maintained publication scripts, committed thesis
exports and canonical `outputs/` define the single authoritative publication
workflow. Previous-environment publication replay, comparison modes, retained
snapshots and migration acceptance/difference artifacts are retired. Publication
does not require them for calculation, validation or website rendering.

`python scripts/check-analysis.py --inputs-only --protected` optionally checks
original thesis input/configuration checksums recorded in the export manifest.
This is a thesis-input integrity check, not an old publication execution mode.
Normal publication validation never opens thesis objects. Historical thesis
reproduction and deliberate export refresh remain separate operations.

Quarto checks the committed exports, runs `python scripts/prepare-workflow.py`
(Python 3.10+ standard library only), then synchronizes the bibliography. The preparation
script regenerates task summaries and the register, validates repository paths,
and copies explicitly linked current files into ignored `repo-files/`. Quarto
copies those downloads into `docs/publication/repo-files/`. A SHA-256 manifest
records the copied source versions. Missing source paths fail the build so a
repository move cannot silently leave an obsolete workflow link.
Downloads whose links have been removed are pruned from both source and rendered
bundles on the next render.

Add current-file links as `[label](repo-files/path/from/repository/root.ext)`.
Only explicitly linked files are bundled; directory walks do not expose model
objects or raw data. Keep historical commit links labeled as history. Linked
downloads are snapshots refreshed on render, not live filesystem editors.
Rmd and QMD documents stay in the repository and are listed as plain paths,
without downloadable copies. Analytical pages retain displayed calculation code.
Figures read canonical `outputs/` files directly; linked CSVs and other review
downloads are copied from their canonical paths into `repo-files/` at render time.

R display chunks read the current saved CSVs on each render. They do not refit
models. Run analytical pipelines separately when new model output is required.

## Documentation ownership

Each `task-*.qmd` owns its scope, inputs, analyses, outputs/findings, decisions
and rationale, implications/handoff, and review state/history. Its included
`sections/` files form part of that record. Keep decisions at their originating
step and link to them from consumers; record a consumer's own use or wording
decision on that consumer's page. Manuscript prose still lives in shared
`sections/` sources, which can contain subsections maintained by different tasks.

F01 followed by RD04 defines the fuel analytical sequence. F01 owns derivation through candidate
evaluation (stages 01-05); RD04 owns prediction/inference, display, and retired
figure history (stages 07-08 and historical 06); RT03 owns fuel Results claims.
This README owns execution instructions; pipeline guides link here and to the
task records.
There is no pipeline map, continuous review page, or separate analytical runner.
Scientific specifications and calculation order live directly in the task pages.
The runtime wrapper explicitly invokes F01 followed by RD04 when both are requested.

Fuel task metadata distinguishes these roles:

- `files`: maintained sources for that step.
- `outputs`: registered produced artifacts; each declared output has one owner.
- `inputs`: consumed upstream artifacts or references, separately linked.
- `depends_on`: upstream task IDs, used for navigation, not scientific acceptance.
- `inline_review`: require every maintained R script and produced CSV to have a
  complete, collapsed review on the owning task page. Consumed files are linked
  rather than included again.

Tasks without these role fields retain their existing related-file lists until
their records are developed; a related-file link does not declare ownership.
The rendered file lists include both owned and consumed files; source documents
are plain path references. Register only explicitly reviewable artifacts; binary model objects and
raw data are not added to downloads by discovering their directories.

F01 and RD04 contain their calculation chunks directly, in document order.
Reusable helpers have complete collapsed source reviews. Calculation chunks have
unique labels, are displayed in both modes, and are checked against QMD source.
CSV reviews preserve every cell. Shared manuscript prose and the companion table
remain presentation includes.

Task selection and calculation order do not use task metadata. The two QMDs
define their calculation order directly. `tasks.json` retains source/output links, summaries, and review
coverage; private model inventories, execution manifests, and per-task
fingerprints are not recorded.

The pre-render preparation rejects duplicate output ownership and invalid
upstream task IDs. Website validation checks complete source and CSV review
coverage separately for F01 and RD04, including literal source/cell comparisons,
as well as links, anchors, and download checksums.

After rendering, run `python publication/scripts/check-website.py` from the
repository root to check local links, task-section anchors, and download
checksums against the current source files.
