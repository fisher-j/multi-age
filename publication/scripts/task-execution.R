# Infrastructure only. Scientific specifications and execution order live in QMD.
publication_setup <- function(selection = character()) {
  root <- normalizePath(Sys.getenv("QUARTO_PROJECT_DIR", unset = getwd()), winslash = "/")
  if (is.null(selection)) selection <- character()
  selection <- unlist(selection, use.names = FALSE)
  if (!is.character(selection) && length(selection)) stop("recompute must be a list of task IDs.")
  tasks <- jsonlite::fromJSON(file.path(root, "tasks.json"), simplifyVector = FALSE)
  supported <- vapply(Filter(function(t) isTRUE(t$recomputable), tasks), `[[`, "", "id")
  if (any(!selection %in% supported)) stop("Unsupported recompute task IDs: ", paste(setdiff(selection, supported), collapse = ", "))
  profile <- strsplit(Sys.getenv("QUARTO_PROFILE"), ",", fixed = TRUE)[[1]]
  recomputing <- "recompute" %in% profile
  if (length(selection) && !recomputing) stop("Nonempty recompute selection requires --profile recompute.")
  context_file <- file.path(root, ".runs", "active-render.json")
  context <- if (file.exists(context_file)) jsonlite::fromJSON(context_file) else list(mode = "review")
  if (recomputing) {
    if (!identical(context$mode, "recompute")) stop("Recomputation requires the project pre-render hook.")
    selection_file <- file.path(context$run, "selection.json")
    if (file.exists(selection_file)) {
      previous <- jsonlite::fromJSON(selection_file)
      if (!setequal(previous, selection)) stop("Selection differs between documents.")
    } else {
      jsonlite::write_json(if (length(selection)) selection else character(), selection_file, auto_unbox = FALSE)
      status <- system2("python", c(shQuote(file.path(root, "scripts", "render-hooks.py")), "select"))
      if (status != 0L) stop("Selection/prerequisite validation failed before calculations.")
    }
  }
  .publication_render <<- list(root = root, selection = selection, tasks = tasks,
                              run = if (recomputing) context$run else NULL)
  invisible(.publication_render)
}

task_selected <- function(id) id %in% .publication_render$selection
task_root <- function(id) {
  if (task_selected(id)) file.path(.publication_render$run, "outputs") else
    file.path(.publication_render$root, "outputs")
}
task_artifact <- function(relative) {
  relative <- sub("^publication/", "", relative)
  owner <- Filter(function(task) {
    patterns <- sub("^publication/", "", c(unlist(task$outputs), unlist(task$private_outputs)))
    any(vapply(patterns, function(pattern) grepl(utils::glob2rx(pattern), relative), logical(1)))
  }, .publication_render$tasks)
  if (length(owner) > 1L) stop("Multiple artifact owners: ", relative)
  path <- if (length(owner) && task_selected(owner[[1]]$id))
    file.path(task_root(owner[[1]]$id), sub("^outputs/", "", relative)) else
    file.path(.publication_render$root, relative)
  if (!file.exists(path)) stop("Required saved artifact missing: ", path, ". Recompute its producing task explicitly.")
  path
}

task_begin <- function(id) {
  stopifnot(task_selected(id))
  Sys.setenv(PUBLICATION_OUTPUT_ROOT = task_root(id))
  source("scripts/publication-paths.R", local = .GlobalEnv)
  jsonlite::write_json(list(pid = Sys.getpid(), r = R.version.string, libraries = .libPaths()),
                       file.path(.publication_render$run, paste0(id, "-r-session.json")),
                       pretty = TRUE, auto_unbox = TRUE)
  status <- system2("python", c("scripts/render-hooks.py", "begin", id))
  if (status != 0L) stop("Could not begin task ", id)
}
task_complete <- function(id) {
  status <- system2("python", c("scripts/render-hooks.py", "complete", id))
  if (status != 0L) stop("Could not record completed task ", id)
}
task_progress <- function(label) {
  status <- system2("python", c("scripts/render-hooks.py", "progress", label))
  if (status != 0L) stop("Could not record step ", label)
}
task_review_state <- function(id) {
  status <- system2("python", c("scripts/render-hooks.py", "review", id), stdout = TRUE)
  if (!is.null(attr(status, "status")) && attr(status, "status") != 0L)
    stop("Could not verify saved input fingerprints for ", id)
  cat(paste(status, collapse = "\n"), "\n")
}
