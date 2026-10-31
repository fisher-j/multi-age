# Collate candidate results. This script does not automatically select a final
# model: AIC leaders remain pending DHARMa and scientific review.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run with Rscript scripts/fuel/05-evaluate-and-select-models.R")
script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
source(file.path(script_dir, "00-fuel-config.R"))

library(dplyr)
library(readr)

paths <- publication_fuel_paths(publication_root)
registry_files <- c(
  file.path(paths$manifests, "phase-component-model-registry.csv"),
  file.path(paths$manifests, "woody-pct-model-registry.csv")
)
if (!all(file.exists(registry_files))) {
  stop("Run 03-fit-phase-components.R and 04-fit-woody-pct-models.R first.")
}

registry <- bind_rows(lapply(registry_files, read_csv, show_col_types = FALSE)) |>
  mutate(
    comparison_group = paste(response, analysis, sep = " | ")
  ) |>
  mutate(
    aic_rank = if_else(fit_status == "fit", rank(aic, ties.method = "min"), NA_real_),
    delta_aic = if_else(
      fit_status == "fit",
      aic - min(aic[fit_status == "fit"], na.rm = TRUE),
      NA_real_
    ),
    .by = comparison_group
  ) |>
  arrange(comparison_group, aic_rank, model_id)

selection_log <- registry |>
  filter(fit_status == "fit", aic_rank == 1) |>
  mutate(
    diagnostic_follow_up = rowSums(
      across(starts_with("dharma_"), ~ !is.na(.x) & .x < 0.05)
    ) > 0
  ) |>
  transmute(
    response,
    analysis,
    provisional_aic_leader = model_id,
    aic,
    pd_hessian,
    dharma_uniformity_p,
    dharma_dispersion_p,
    dharma_outlier_p,
    dharma_zero_inflation_p,
    selection_status = case_when(
      !pd_hessian ~ "rejected pending convergence resolution",
      diagnostic_follow_up ~ "requires diagnostic follow-up",
      TRUE ~ "eligible for scientific review"
    ),
    selection_rationale = "This is the AIC-leading predeclared candidate, not a final selection. Retain only after diagnostic and scientific review."
  )

write_csv(registry, file.path(paths$manifests, "model-comparison.csv"))
write_csv(selection_log, file.path(paths$manifests, "model-selection-log.csv"))
