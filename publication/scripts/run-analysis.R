# Explicit calculations; ordinary website renders only review saved outputs.
args <- commandArgs(trailingOnly = TRUE)
command <- if (length(args)) args[1] else "check"
if (!command %in% c("restore", "f01", "rd04", "fuel", "regeneration", "all", "check"))
  stop("Use restore, f01, rd04, fuel, regeneration, all, or check.")
root <- normalizePath(getwd(), winslash = "/")
stopifnot(file.exists(file.path(root, "_quarto.yml")), getRversion() == "4.6.2")
source(".Rprofile")
if (command == "restore") {
  renv::restore(prompt = FALSE)
  quit(status = 0L)
}
source("scripts/verify-environment.R")
verify_publication_environment()
check <- function(arguments) {
  if (system2("python", c("scripts/check-analysis.py", arguments)) != 0L)
    stop("Publication verification failed; saved outputs were not replaced.")
}
check("--inputs-only")
if (command %in% c("f01", "rd04", "fuel", "all")) {
  tasks <- switch(command, f01 = "f01", rd04 = "rd04", c("f01", "rd04"))
  for (task in tasks) {
    status <- system2("quarto", c("render", paste0("task-", task, ".qmd"),
      "--to", "html", "-P", "recompute:true", "--output-dir", paste0(".runs/", task, "/site")))
    if (status != 0L) stop(toupper(task), " recomputation failed; inspect publication/.runs/", task, ".")
  }
  if (command != "all") quit(status = 0L)
  command <- "regeneration"
}
if (command == "check") {
  check(c("--outputs", "outputs"))
  quit(status = 0L)
}
staging <- file.path(root, ".runs", "regeneration")
output <- file.path(staging, "outputs")
unlink(output, recursive = TRUE)
dir.create(output, recursive = TRUE)
Sys.setenv(PUBLICATION_OUTPUT_ROOT = output)
capture.output(sessionInfo(), file = file.path(staging, "session-info.txt"))
# Regeneration retains its separate scientific sequence.
for (script in c("01-generate-rd02-display.R", "02-compare-rd02-random-effects.R",
                 "03-compare-rd02-separate-species.R", "05-fit-selected-species-models.R",
                 "04-assemble-rd02-sensitivity.R")) {
  message("Running regeneration/", script)
  status <- system2(file.path(R.home("bin"), "Rscript.exe"),
                    shQuote(file.path("scripts", "regeneration", script)))
  if (status != 0L) stop("Stage failed: ", script, ". Saved outputs were not replaced; inspect ", staging)
}
check(c("--outputs", shQuote(output), "--domains", "regeneration", "--require-models"))
for (file in list.files(output, recursive = TRUE, full.names = TRUE)) {
  target <- file.path(root, "outputs", substring(file, nchar(output) + 2L))
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(file, target, overwrite = TRUE))
}
message("Checked and copied regeneration outputs.")
