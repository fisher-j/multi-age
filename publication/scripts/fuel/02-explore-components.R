# Explore publication fuel components before model fitting.
#
# This script fixes the random-intercept structure from the experimental design:
# site, block nested within site (site:treatment), and corner nested within
# block. It does not select, fit, or simplify random effects.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run with Rscript scripts/fuel/02-explore-components.R")
}

script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
source(file.path(script_dir, "00-fuel-config.R"))

library(dplyr)
library(readr)

paths <- publication_fuel_paths(publication_root)
components <- readRDS(file.path(paths$data, "fuel-components-transect.rds"))
paired <- readRDS(file.path(paths$data, "fuel-paired-transect.rds"))

add_design_groups <- function(data) {
  data |>
    mutate(
      block = interaction(site, treatment, drop = TRUE),
      design_corner = interaction(site, treatment, corner, drop = TRUE)
    )
}

component_data <- components |>
  mutate(component = as.character(component)) |>
  add_design_groups()

component_summary <- component_data |>
  summarise(
    n_transects = n(),
    n_sites = n_distinct(site),
    n_blocks = n_distinct(block),
    n_corners = n_distinct(design_corner),
    n_zero = sum(load_mgha == 0, na.rm = TRUE),
    prop_zero = mean(load_mgha == 0, na.rm = TRUE),
    n_missing = sum(is.na(load_mgha)),
    mean_mgha = mean(load_mgha, na.rm = TRUE),
    sd_mgha = sd(load_mgha, na.rm = TRUE),
    variance_mgha = var(load_mgha, na.rm = TRUE),
    cv = if_else(mean_mgha > 0, sd_mgha / mean_mgha, NA_real_),
    min_mgha = min(load_mgha, na.rm = TRUE),
    q25_mgha = quantile(load_mgha, 0.25, na.rm = TRUE, names = FALSE),
    median_mgha = median(load_mgha, na.rm = TRUE),
    q75_mgha = quantile(load_mgha, 0.75, na.rm = TRUE, names = FALSE),
    max_mgha = max(load_mgha, na.rm = TRUE),
    .by = c("phase", "component", "treatment")
  ) |>
  arrange(phase, component, treatment)

component_candidates <- component_data |>
  summarise(
    n_zero = sum(load_mgha == 0, na.rm = TRUE),
    n_transects = n(),
    n_missing = sum(is.na(load_mgha)),
    .by = c("phase", "component")
  ) |>
  mutate(
    response_type = "nonnegative continuous load",
    fixed_effects = "treatment",
    random_effects = "(1 | site) + (1 | site:treatment) + (1 | site:treatment:corner)",
    conditional_family = if_else(n_zero > 0, "ziGamma(link = log)", "Gamma(link = log)"),
    zero_inflation_candidates = if_else(
      n_zero > 0,
      "~1; consider ~treatment only if zero-process diagnostics and replication support it",
      "none unless later diagnostics identify a structural-zero process"
    ),
    dispersion_candidates = "~1 first; compare ~treatment only if residual dispersion diagnostics support it",
    status = if_else(
      n_missing > 0,
      "candidate specifications only; resolve missing values before model fitting",
      "candidate specifications only; no model fitted"
    )
  ) |>
  arrange(phase, component)

paired_data <- paired |>
  add_design_groups()

paired_summary <- paired_data |>
  summarise(
    n_pairs = n(),
    n_sites = n_distinct(site),
    n_blocks = n_distinct(block),
    n_corners = n_distinct(design_corner),
    n_negative = sum(woody_standing_reduction_mgha < 0, na.rm = TRUE),
    n_zero = sum(woody_standing_reduction_mgha == 0, na.rm = TRUE),
    n_positive = sum(woody_standing_reduction_mgha > 0, na.rm = TRUE),
    mean_mgha = mean(woody_standing_reduction_mgha, na.rm = TRUE),
    sd_mgha = sd(woody_standing_reduction_mgha, na.rm = TRUE),
    min_mgha = min(woody_standing_reduction_mgha, na.rm = TRUE),
    median_mgha = median(woody_standing_reduction_mgha, na.rm = TRUE),
    max_mgha = max(woody_standing_reduction_mgha, na.rm = TRUE),
    .by = treatment
  ) |>
  arrange(treatment)

paired_candidate <- tibble(
  response = "woody_standing_reduction_mgha",
  response_type = "signed matched pre-PCT minus post-PCT contrast",
  fixed_effects = "treatment",
  random_effects = "(1 | site) + (1 | site:treatment) + (1 | site:treatment:corner)",
  conditional_family = "Do not use Gamma automatically; select a signed-response model after distribution diagnostics.",
  zero_inflation_candidates = "Not applicable to a signed contrast without a separate two-part estimand.",
  dispersion_candidates = "Assess residual dispersion after selecting a signed-response family.",
  status = "candidate specification only; no model fitted"
)

write_csv(component_summary, file.path(paths$data, "fuel-component-exploration-summary.csv"))
write_csv(component_candidates, file.path(paths$manifests, "fuel-component-model-candidates.csv"))
write_csv(paired_summary, file.path(paths$data, "woody-pct-contrast-exploration-summary.csv"))
write_csv(paired_candidate, file.path(paths$manifests, "woody-pct-contrast-model-candidate.csv"))
