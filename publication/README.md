# Publication website maintenance

The publication is an independent Quarto website rendered to `docs/publication/`.
Its top navigation contains **Development workflow** and **Manuscript draft**,
each with a dedicated sidebar. The repository's legacy products are separate.

- `workflow.qmd`: process overview, repository map, and documentation responsibilities.
- `pipeline-fuel.qmd`: compact execution/dependency map linking authoritative steps.
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
```

The wrapper explicitly selects publication R 4.6.2 and activates this directory's
independent `renv` project. The legacy products explicitly select thesis R 4.3.2.
`PUBLICATION_R_HOME` and `THESIS_R_HOME` can override installation locations;
neither wrapper changes the machine PATH. An explicit `R_LIBS_USER` is preserved.
Actual namespace loads and installed DESCRIPTION versions are checked against
the publication lock; broad startup package scans are disabled because sandbox
permission checks can misreport missing packages.

## Independent calculations

From the repository root:

```bat
run-publication.bat restore
run-publication.bat all
run-publication.bat check
render-products.bat publication --to html
python publication/scripts/check-website.py
```

`fuel` and `regeneration` run their respective pipelines. Fuel order is
01, 02, 03, 04, 05, 07, 08; regeneration order is 01, 02, 03, 05, 04.
The runner uses committed exports under `inputs/thesis/`, scripts under
`scripts/fuel/` and `scripts/regeneration/`, and canonical outputs under
`outputs/`. Thesis model objects and thesis renv are not calculation dependencies.
See [the input contract](inputs/thesis/README.md) for the separate export operation.

Recomputation starts with empty model/output directories under `.runs/`.
Each stage stops on failure. Existing numerical checks verify predictions,
gradients, integration, covariance and assembly within the owning scripts.
Before promotion, validation checks declared outputs, complete CSV structure,
finite estimates, ordered intervals, nonnegative SEs and registered fuel-model
files. `check` validates saved outputs without requiring local model caches;
`--require-models` additionally checks fitted-model files after recomputation.
The runner refuses to overwrite an existing run directory; archive it before
a deliberate rerun.

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

Quarto first runs `python scripts/prepare-workflow.py` (Python 3.10+ standard
library only), then the existing bibliography synchronization. The preparation
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

R display chunks read the current saved CSVs on each render. They do not refit
models. Run analytical pipelines separately when new model output is required.

## Documentation ownership

Each `task-*.qmd` owns its scope, inputs, analyses, outputs/findings, decisions
and rationale, implications/handoff, and review state/history. Its included
`sections/` files form part of that record. Keep decisions at their originating
step and link to them from consumers; record a consumer's own use or wording
decision on that consumer's page. Manuscript prose still lives in shared
`sections/` sources, which can contain subsections maintained by different tasks.

The fuel map links the complete sequence. F01 owns derivation through candidate
evaluation (stages 01-05); RD04 owns prediction/inference, display, and retired
figure history (stages 07-08 and historical 06); RT03 owns fuel Results claims.
The pipeline README contains execution instructions and links to those records.
There is no continuous review page. If a combined view later improves code-agent
work, build it with Quarto includes of the step-owned sections, never copies.

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
The rendered file lists and download bundle include both owned and consumed
files. Register only explicitly reviewable artifacts; binary model objects and
raw data are not added to downloads by discovering their directories.

F01 includes `sections/f01-pipeline-review.qmd`; RD04 includes
`sections/rd04-inference-review.qmd` and `sections/rd04-source-review.qmd`.
Original R scripts are included in plain code fences. The RD04 Lua filter nests
the retired README's headings. CSV chunks read each step's `outputs` metadata
using `scripts/render-csv-review.R`, preserving every saved cell. Add source
includes to the owning review section and register its files/outputs there.

The pre-render preparation rejects duplicate output ownership and invalid
upstream task IDs. Website validation checks complete source and CSV review
coverage separately for F01 and RD04, including literal source/cell comparisons,
as well as links, anchors, and download checksums.

After rendering, run `python publication/scripts/check-website.py` from the
repository root to check local links, task-section anchors, and download
checksums against the current source files.
