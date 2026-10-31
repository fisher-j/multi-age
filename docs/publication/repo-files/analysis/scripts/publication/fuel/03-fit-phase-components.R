# Fit a predeclared, minimal candidate set for phase-specific fuel components.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run with Rscript scripts/publication/fuel/03-fit-phase-components.R")
script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
analysis_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
source(file.path(script_dir, "00-fuel-config.R"))
source(file.path(script_dir, "03-model-utils.R"))

library(dplyr)
library(purrr)
library(readr)

paths <- publication_fuel_paths(analysis_root)
output_paths <- model_output_paths(paths)
# This stage owns all pre/post phase-component artifacts. Clear only its prior
# artifacts so an idempotent rerun cannot retain a candidate no longer tested.
for (directory in c(output_paths$models, output_paths$diagnostics)) {
  stale <- list.files(directory, pattern = "^(pre|post)pct_", full.names = TRUE)
  if (length(stale)) file.remove(stale)
}
components <- readRDS(file.path(paths$data, "fuel-components-transect.rds")) |>
  mutate(
    component = as.character(component),
    block = interaction(site, treatment, drop = TRUE),
    design_corner = interaction(site, treatment, corner, drop = TRUE)
  )

design_formula <- load_mgha ~ treatment +
  (1 | site) + (1 | block) + (1 | design_corner)

fit_component_candidates <- function(data, phase_value, component_value) {
  model_data <- data |>
    filter(
      .data$phase == .env$phase_value,
      .data$component == .env$component_value,
      !is.na(load_mgha)
    )
  zero_count <- sum(model_data$load_mgha == 0)
  prefix <- paste(phase_value, component_value, sep = "_")

  candidates <- list()
  if (zero_count > 0) {
    candidates[["base"]] <- list(glmmTMB::ziGamma(link = "log"), "ziGamma(link = log)", ~1, "~1", ~1, "~1")
    candidates[["disp_treatment"]] <- list(glmmTMB::ziGamma(link = "log"), "ziGamma(link = log)", ~1, "~1", ~treatment, "~treatment")
    if (zero_count >= 8) {
      candidates[["zi_treatment"]] <- list(glmmTMB::ziGamma(link = "log"), "ziGamma(link = log)", ~treatment, "~treatment", ~1, "~1")
    }
  } else {
    candidates[["base"]] <- list(Gamma(link = "log"), "Gamma(link = log)", ~0, "~0", ~1, "~1")
    candidates[["disp_treatment"]] <- list(Gamma(link = "log"), "Gamma(link = log)", ~0, "~0", ~treatment, "~treatment")
  }

  # The thesis used site plus treatment in both the zero and dispersion
  # submodels for post-PCT 100-hour fuels. Retain that candidate and add one
  # deliberately more complex site-by-treatment dispersion sensitivity check.
  if (phase_value == "postpct" && component_value == "hundhr") {
    candidates[["zi_treatment_site_disp_treatment_site"]] <- list(
      glmmTMB::ziGamma(link = "log"), "ziGamma(link = log)",
      ~treatment + site, "~treatment + site",
      ~treatment + site, "~treatment + site"
    )
    candidates[["zi_treatment_site_disp_treatment_site_interaction"]] <- list(
      glmmTMB::ziGamma(link = "log"), "ziGamma(link = log)",
      ~treatment + site, "~treatment + site",
      ~treatment * site, "~treatment * site"
    )
  }

  imap_dfr(candidates, function(candidate, candidate_name) {
    fit_and_diagnose(
      model_id = paste(prefix, candidate_name, sep = "_"),
      response = component_value,
      analysis = paste(phase_value, "phase-specific component", sep = ": "),
      data = model_data,
      formula = design_formula,
      family = candidate[[1]],
      family_label = candidate[[2]],
      ziformula = candidate[[3]],
      ziformula_label = candidate[[4]],
      dispformula = candidate[[5]],
      dispformula_label = candidate[[6]],
      paths = paths
    )
  })
}

registry <- components |>
  distinct(phase, component) |>
  arrange(phase, component) |>
  pmap_dfr(function(phase, component) {
    fit_component_candidates(components, phase, component)
  })

write_csv(registry, file.path(paths$manifests, "phase-component-model-registry.csv"))
