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

The Windows wrapper uses the installed R user library under
`%USERPROFILE%\AppData\Local\R\win-library\<R major.minor>` when
`R_LIBS_USER` is unset. This avoids sandbox sessions redirecting R's default
library to an empty `LOCALAPPDATA` location. An explicit `R_LIBS_USER` is
preserved. If Quarto reports missing `knitr` or `rmarkdown`, check the library
paths before reinstalling packages; see the repository's `AGENTS.md` for
diagnostic and direct-command instructions. Publication rendering does not
activate the separate `analysis/renv` environment.

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
