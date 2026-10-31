# Small infrastructure checks; run with publication R from publication/.
source(".Rprofile")
source("scripts/task-execution.R")
old_profile <- Sys.getenv("QUARTO_PROFILE")
Sys.unsetenv("QUARTO_PROFILE")
stopifnot(inherits(try(publication_setup("F01"), silent = TRUE), "try-error"),
          inherits(try(publication_setup("UNKNOWN"), silent = TRUE), "try-error"))
Sys.setenv(QUARTO_PROFILE = old_profile)

test_root <- tempfile("task-artifacts-")
dir.create(test_root)
test_run <- file.path(test_root, ".runs", "test")
for (base in c(test_root, test_run)) dir.create(file.path(base, "outputs", "fuel"), recursive = TRUE)
writeLines("saved F01", file.path(test_root, "outputs/fuel/data.csv"))
writeLines("staged F01", file.path(test_run, "outputs/fuel/data.csv"))
writeLines("saved RD04", file.path(test_root, "outputs/fuel/estimates.csv"))
.publication_render <- list(root = test_root, run = test_run, selection = "F01", tasks = list(
  list(id = "F01", outputs = "publication/outputs/fuel/data.csv", private_outputs = "publication/outputs/fuel/models/*.rds"),
  list(id = "RD04", outputs = "publication/outputs/fuel/estimates.csv")))
stopifnot(readLines(task_artifact("outputs/fuel/data.csv")) == "staged F01",
          readLines(task_artifact("outputs/fuel/estimates.csv")) == "saved RD04")
unlink(file.path(test_run, "outputs/fuel/data.csv"))
stopifnot(inherits(try(task_artifact("outputs/fuel/data.csv"), silent = TRUE), "try-error"))
.publication_render$selection <- character()
stopifnot(readLines(task_artifact("outputs/fuel/data.csv")) == "saved F01",
          readLines(task_artifact("outputs/fuel/estimates.csv")) == "saved RD04",
          !dir.exists(file.path(test_root, "outputs/fuel/models")))
message("R mode, staged routing, missing-output, and cache-free review checks passed.")
