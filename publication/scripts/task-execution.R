# Task-local scratch outputs only; scientific calculations live in QMD order.
prepare_fuel_task <- function(task) {
  stopifnot(task %in% c("F01", "RD04"))
  source("scripts/verify-environment.R")
  verify_publication_environment()
  if (system2("python", c("scripts/check-analysis.py", "--inputs-only")) != 0L)
    stop("Required publication inputs are missing or modified.")
  if (task == "RD04") {
    registry <- "outputs/fuel/manifests/model-comparison.csv"
    selection <- "outputs/fuel/manifests/model-selection-log.csv"
    if (!all(file.exists(c(registry, selection))))
      stop("RD04 requires saved F01 model registers. Run run-publication.bat f01.")
    fits <- read.csv(registry, stringsAsFactors = FALSE)
    models <- file.path("outputs/fuel/models", paste0(fits$model_id[fits$fit_status == "fit"], ".rds"))
    missing <- models[!file.exists(models)]
    if (length(missing)) stop("RD04 requires saved F01 fitted models: ",
      paste(missing, collapse = ", "), ". Run run-publication.bat f01 to refit.")
  }
  output <- file.path(normalizePath(getwd(), winslash = "/"), ".runs", tolower(task), "outputs")
  # This fixed task directory is disposable, including after a failed attempt.
  unlink(output, recursive = TRUE)
  dir.create(output, recursive = TRUE)
  Sys.setenv(PUBLICATION_OUTPUT_ROOT = output)
  source("scripts/publication-paths.R", local = .GlobalEnv)
  output
}

publish_fuel_task <- function(task, output) {
  status <- system2("python", c("scripts/check-analysis.py", "--outputs", shQuote(output),
                               "--domains", "fuel", "--task", task, "--require-models"))
  if (status != 0L) stop(task, " output checks failed; saved outputs were not replaced.")
  for (file in list.files(output, recursive = TRUE, full.names = TRUE)) {
    target <- file.path("outputs", substring(file, nchar(output) + 2L))
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file, target, overwrite = TRUE)) stop("Could not copy output: ", target)
  }
  message("Checked and copied ", task, " outputs.")
}
