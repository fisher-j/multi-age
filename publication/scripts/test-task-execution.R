# Behavioral checks for task scratch reuse, failed checks, copying and saved models.
source(".Rprofile")
source("scripts/task-execution.R")
project <- getwd()
fixture <- tempfile("fuel-task-")
dir.create(file.path(fixture, "scripts"), recursive = TRUE)
dir.create(file.path(fixture, "inputs/thesis"), recursive = TRUE)
file.copy("scripts/check-analysis.py", file.path(fixture, "scripts"))
file.copy("scripts/publication-paths.R", file.path(fixture, "scripts"))
writeLines("verify_publication_environment <- function() TRUE",
           file.path(fixture, "scripts/verify-environment.R"))
writeLines('{"exports": {}, "inputs": {}}', file.path(fixture, "inputs/thesis/manifest.json"))
writeLines('[{"id":"F01","outputs":["publication/outputs/fuel/estimates.csv"]},
             {"id":"RD04","outputs":["publication/outputs/fuel/rd04.csv"]}]',
           file.path(fixture, "tasks.json"))
tryCatch({
  setwd(fixture)
  dir.create("outputs/fuel", recursive = TRUE)
  writeLines("estimate,se,lcl,ucl\n1,0.1,0.8,1.2", "outputs/fuel/estimates.csv")
  output <- prepare_fuel_task("F01")
  dir.create(file.path(output, "fuel"))
  writeLines("estimate,se,lcl,ucl\n2,0.1,3,4", file.path(output, "fuel/estimates.csv"))
  stopifnot(inherits(try(publish_fuel_task("F01", output), silent = TRUE), "try-error"),
            read.csv("outputs/fuel/estimates.csv")$estimate == 1)
  writeLines("estimate,se,lcl,ucl\n2,0.1,1.8,2.2", file.path(output, "fuel/estimates.csv"))
  publish_fuel_task("F01", output)
  stopifnot(read.csv("outputs/fuel/estimates.csv")$estimate == 2,
            !file.exists("outputs/fuel/rd04.csv"))
  # A previous failed attempt needs no manual archive, and cannot supply stale files.
  writeLines("stale", file.path(output, "stale.csv"))
  stopifnot(prepare_fuel_task("F01") == output, length(list.files(output)) == 0L)
  dir.create("outputs/fuel/manifests")
  writeLines("model_id,fit_status\nrequired,fit", "outputs/fuel/manifests/model-comparison.csv")
  writeLines("model_id\nrequired", "outputs/fuel/manifests/model-selection-log.csv")
  error <- tryCatch(prepare_fuel_task("RD04"), error = conditionMessage)
  stopifnot(grepl("RD04 requires saved F01 fitted models", error, fixed = TRUE),
            grepl("required.rds", error, fixed = TRUE))
}, finally = {
  setwd(project)
  Sys.unsetenv("PUBLICATION_OUTPUT_ROOT")
  unlink(fixture, recursive = TRUE)
})
message("Scratch reuse, failed-check preservation, task-only copying and missing-model checks passed.")
