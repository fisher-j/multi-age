# Publication website maintenance

The publication is an independent Quarto website rendered to `docs/publication/`.
Its top navigation contains **Development workflow** and **Manuscript draft**,
each with a dedicated sidebar. The repository's legacy products are separate.

- `workflow.qmd`: process overview, repository map, and maintenance instructions.
- `tasks.json`: task statuses, targets, summaries of the dedicated task records,
  and repository file paths.
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

F01 references the pipeline Markdown and R files in place with Quarto `include`
shortcodes in `sections/f01-pipeline-review.qmd`. Script includes are inside plain
R code fences for display. A page-specific Lua filter nests the README headings
without editing its source. The CSV display chunk reads the F01 output paths in
`tasks.json` directly using `scripts/render-csv-review.R`; it preserves all rows,
columns, and saved numeric text. No intermediate copies of source contents are
written into QMD files. Add new script includes to the review section and register
new scripts or CSV outputs in `tasks.json`.

After rendering, run `python publication/scripts/check-website.py` from the
repository root to check local links, task-section anchors, and download
checksums against the current source files.
