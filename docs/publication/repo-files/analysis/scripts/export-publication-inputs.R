# Refresh the publication boundary using the unchanged thesis environment.
# From analysis/: Rscript scripts/export-publication-inputs.R
stopifnot(getRversion() == "4.3.2")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(glmmTMB); library(marginaleffects)
})
stopifnot(requireNamespace("digest", quietly = TRUE), requireNamespace("jsonlite", quietly = TRUE))
analysis_root <- normalizePath(".")
stopifnot(file.exists("renv.lock"), grepl("analysis/renv/library", .libPaths()[1], fixed = TRUE))
output <- normalizePath("../publication", mustWork = TRUE)
output <- file.path(output, "inputs", "thesis")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
inputs <- c("summarize_collected_data.rda", "calculate_fuel_load.rda",
            "fuel_modeling_round_3.rda", "regen_visualize.rda", "renv.lock", ".Rprofile")
sha <- function(path) digest::digest(readBin(path, "raw", n = file.info(path)$size),
                                    serialize = FALSE, algo = "sha256")
protected <- vapply(inputs, sha, character(1))
e <- new.env(); load(inputs[1], envir = e)
columns <- c("phase", "site", "treatment", "corner", "azi",
             grep("^(duff_litter|pct_litter|fbd|live_woody|dead_woody|avg_w_ht|live_herb|dead_herb|avg_h_ht)[12]$",
                  names(e$d$transects), value = TRUE))
measurements <- as.data.frame(e$d$transects[, columns])
saveRDS(measurements, file.path(output, "fuel-measurements.rds"), version = 3)
e <- new.env(); load(inputs[2], envir = e)
loads <- as.data.frame(e$total_load[, c("phase", "site", "treatment", "corner", "azi",
                                      "onehr", "tenhr", "hundhr", "thoushr")])
saveRDS(loads, file.path(output, "fuel-deadwood-loads.rds"), version = 3)
e <- new.env(); load(inputs[4], envir = e)
regen <- as.data.frame(e$regen_pa)
saveRDS(regen, file.path(output, "regeneration-measurements.rds"), version = 3)
stopifnot(nrow(measurements) == 256L, nrow(loads) == 256L, nrow(regen) == 256L)
source("scripts/publication-export/fuel-historical-reference.R")
source("scripts/publication-export/rd02-historical-reference.R")
export_fuel_reference(analysis_root, output)
export_rd02_reference(e, output)
datasets <- list(fuel_measurements = measurements, fuel_deadwood_loads = loads,
                 regeneration_measurements = regen)
dictionary <- bind_rows(lapply(names(datasets), function(name) {
  x <- datasets[[name]]
  data.frame(dataset = name, column = names(x),
    type = vapply(x, function(v) paste(class(v), collapse = "/"), character(1)),
    factor_levels = vapply(x, function(v) paste(levels(v), collapse = ";"), character(1)),
    missing = vapply(x, function(v) sum(is.na(v)), integer(1)))
}))
write.csv(dictionary, file.path(output, "data-dictionary.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(output, "export-session-info.txt"))
files <- list.files(output, recursive = TRUE, full.names = TRUE)
files <- files[basename(files) != "manifest.json"]
scripts <- c("scripts/export-publication-inputs.R", list.files("scripts/publication-export", full.names = TRUE))
manifest <- list(schema_version = 1L, r = R.version.string,
  inputs = as.list(protected), scripts = as.list(vapply(scripts, sha, character(1))),
  exports = setNames(as.list(vapply(files, sha, character(1))),
                     substring(files, nchar(output) + 2L)),
  units = list(depth = "cm", vegetation_height = "m", cover = "percent",
               fuel_load = "Mg ha-1", basal_area = "m2 ha-1"),
  contract = "Preserve order, identifiers, values, missingness, types, factors and contrasts; no thesis model objects exported")
jsonlite::write_json(manifest, file.path(output, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
stopifnot(identical(protected, vapply(inputs, sha, character(1))))
cat("Exported thesis boundary without modifying protected inputs.\n")
