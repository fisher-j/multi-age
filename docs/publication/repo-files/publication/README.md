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
```

The wrapper explicitly selects publication R 4.6.2 and activates this directory's
independent `renv` project. The legacy products explicitly select thesis R 4.3.2.
`PUBLICATION_R_HOME` and `THESIS_R_HOME` can override installation locations;
neither wrapper changes the machine PATH. An explicit `R_LIBS_USER` is preserved.
Actual namespace loads and installed DESCRIPTION versions are checked against
the publication lock; broad startup package scans are disabled because sandbox
permission checks can misreport missing packages.

## Independent calculations and selective review

The [fuel operational guide](scripts/fuel/README.md) documents native Quarto
profiles, whole-task selection, staging, prerequisites and promotion. Review is
the default: calculation code is displayed while evidence is read from saved
artifacts; fitted-model caches are unnecessary. Recompute F01 and RD04 with:

```bat
render-products.bat publication --profile recompute --execute-params recompute-fuel.yml --to html
```

A native parameter file such as `recompute: [RD04]` changes only selected tasks.
Unselected pages review canonical outputs; selected producers supply staged
outputs to all consumers. Fingerprints identify changed upstream inputs without
silently recomputing downstream tasks or accepting models. A selected producer's
missing outputs are errors, never canonical fallbacks.

Pre-render hooks check exports/environment and prepare staging. Native execution
parameters are checked in shared document setup before calculations. Post-render
hooks validate outputs and private model/data files, refresh staged downloads,
then promote selected tasks only. Failed renders do not promote. HTML remains in
`.runs/<run-name>/site`; render normally after promotion for the canonical site.
Use `--output-dir .runs/<new-run-name>/site` for another run; existing execution
records are never overwritten.

`run-publication.bat fuel` delegates to this render. `all` runs fuel followed by
regeneration, validating/promoting each domain independently. `regeneration`
retains its existing sequence (01, 02, 03, 05, 04), staging and checks.
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

F01 followed by RD04 defines the fuel analytical sequence. F01 owns derivation through candidate
evaluation (stages 01-05); RD04 owns prediction/inference, display, and retired
figure history (stages 07-08 and historical 06); RT03 owns fuel Results claims.
The pipeline README contains execution instructions and links to those records.
There is no pipeline map, continuous review page, or separate analytical runner.
Scientific specifications and orchestration live directly in the task pages.

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

F01 and RD04 contain their calculation chunks directly, in document order.
Reusable helpers have complete collapsed source reviews. Calculation chunks have
unique labels, are displayed in both modes, and are checked against QMD source.
CSV reviews preserve every cell. Shared manuscript prose and the companion table
remain presentation includes.

`recomputable` opts a task into selection; `calculation_chunks` records its ordered
labels. `private_inputs` and `private_outputs` inventory model/data files and
execution manifests without adding downloads. Selected tasks record source and
input artifact hashes. QMD parameter defaults are declared in each document for
knitr; the project uses the same empty-list default.
`execution_manifest` names each task's private provenance record. Reusable
rendering infrastructure does not determine the task's scientific run order.

QMD downloads use `.qmd.txt` so Quarto treats them as source downloads, not extra
render targets; their manifest records the original repository path.

The pre-render preparation rejects duplicate output ownership and invalid
upstream task IDs. Website validation checks complete source and CSV review
coverage separately for F01 and RD04, including literal source/cell comparisons,
as well as links, anchors, and download checksums.

After rendering, run `python publication/scripts/check-website.py` from the
repository root to check local links, task-section anchors, and download
checksums against the current source files.
