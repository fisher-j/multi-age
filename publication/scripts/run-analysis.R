# Explicit execution boundary, separate from the website's saved-output display.
args <- commandArgs(trailingOnly = TRUE)
command <- if (length(args)) args[1] else "check"
stopifnot(command %in% c("restore", "fuel", "regeneration", "all", "check"))
root <- normalizePath(getwd(), winslash = "/")
stopifnot(file.exists(file.path(root, "_quarto.yml")), getRversion() == "4.6.2")
source(".Rprofile")
if (command == "restore") {
  renv::restore(prompt = FALSE)
  quit(status = 0L)
}
if (command %in% c("fuel", "all")) {
  # Quarto alone owns document execution/order; this is a compatibility alias.
  status <- system2("quarto", c("render", "--profile", "recompute", "--to", "html",
                               "--execute-params", "recompute-fuel.yml"))
  if (status != 0L) {
    system2("python", c("scripts/render-hooks.py", "failed"))
    stop("Fuel recompute render failed; inspect publication/.runs/recompute.")
  }
  if (command == "fuel") quit(status = 0L)
  command <- "regeneration"
}
stopifnot(grepl(paste0(root, "/renv/library/"),
               normalizePath(.libPaths()[1], winslash = "/"), fixed = TRUE),
          !any(grepl("analysis/renv", .libPaths(), fixed = TRUE)))
packages <- trimws(strsplit(gsub("[\r\n]", "", read.dcf("DESCRIPTION")[1, "Imports"]), ",")[[1]])
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Required packages unavailable: ", paste(missing, collapse = ", "),
                          ". Run run-publication.bat restore.")
lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)
for (package in names(lock$Packages)) {
  descriptions <- file.path(.libPaths(), package, "DESCRIPTION")
  descriptions <- descriptions[file.exists(descriptions)]
  installed <- if (length(descriptions)) unname(read.dcf(descriptions[1], fields = "Version")[1, 1]) else NA_character_
  if (!identical(installed, lock$Packages[[package]]$Version))
    stop("Package differs from publication lock: ", package, ". Run restore.")
}
check <- function(arguments) {
  status <- system2("python", c("scripts/check-analysis.py", arguments))
  if (status != 0L) stop("Publication verification failed; canonical outputs were not replaced.")
}
check("--inputs-only")
if (command == "check") {
  check(c("--outputs", "outputs"))
  quit(status = 0L)
}
staging <- file.path(root, ".runs", command)
output <- file.path(staging, "outputs")
if (dir.exists(staging)) stop("Staging directory already exists: ", staging,
                             ". Inspect/archive it before rerunning this command.")
dir.create(output, recursive = TRUE)
Sys.setenv(PUBLICATION_OUTPUT_ROOT = output)
sequences <- list(regeneration = c("01-generate-rd02-display.R", "02-compare-rd02-random-effects.R",
    "03-compare-rd02-separate-species.R", "05-fit-selected-species-models.R",
    "04-assemble-rd02-sensitivity.R"))
domains <- command
capture.output(sessionInfo(), file = file.path(staging, "session-info.txt"))
execution <- list(command = command, r = R.version.string, libraries = .libPaths(),
  lock_md5 = unname(tools::md5sum("renv.lock")),
  input_manifest_md5 = unname(tools::md5sum("inputs/thesis/manifest.json")),
  status = "running", completed_stages = character())
record_execution <- function() jsonlite::write_json(execution,
  file.path(staging, "execution.json"), pretty = TRUE, auto_unbox = TRUE)
record_execution()
for (domain in domains) for (script in sequences[[domain]]) {
  message("Running ", domain, "/", script)
  status <- system2(file.path(R.home("bin"), "Rscript.exe"),
                    shQuote(file.path("scripts", domain, script)))
  if (status != 0L) {
    execution$status <- "stage_failed"
    execution$failed_stage <- paste(domain, script, sep = "/")
    record_execution()
    stop("Stage failed: ", script, ". Candidate outputs retained at ", staging)
  }
  execution$completed_stages <- c(execution$completed_stages, paste(domain, script, sep = "/"))
  record_execution()
}
tryCatch(check(c("--outputs", shQuote(output), "--domains", paste(domains, collapse = ","),
                "--require-models", "--report", shQuote(file.path(staging, "validation.json")))),
  error = function(error) {
    execution$status <<- "output_validation_failed"
    record_execution()
    stop(error)
  })
# Only verified numerical outputs reach the canonical tree.
for (domain in domains) {
  files <- list.files(file.path(output, domain), recursive = TRUE, full.names = TRUE)
  for (file in files) {
    relative <- substring(file, nchar(output) + 2L)
    target <- file.path(root, "outputs", relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file, target, overwrite = TRUE))
  }
}
execution$status <- "verified_and_promoted"
record_execution()
message("Validated and promoted publication outputs; run record: ", staging)
