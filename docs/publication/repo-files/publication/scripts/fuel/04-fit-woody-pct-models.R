# Fit complementary woody PCT models: phase-specific standing load and signed
# matched pre-minus-post reduction.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run with Rscript scripts/fuel/04-fit-woody-pct-models.R")
script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
source(file.path(script_dir, "00-fuel-config.R"))
source(file.path(script_dir, "03-model-utils.R"))

library(dplyr)
library(readr)
library(tidyr)

paths <- publication_fuel_paths(publication_root)
output_paths <- model_output_paths(paths)
# This stage owns all repeated-woody and paired-contrast artifacts.
for (directory in c(output_paths$models, output_paths$diagnostics)) {
  stale <- list.files(directory, pattern = "^woody_", full.names = TRUE)
  if (length(stale)) file.remove(stale)
}
paired <- readRDS(file.path(paths$data, "fuel-paired-transect.rds")) |>
  mutate(
    block = interaction(site, treatment, drop = TRUE),
    design_corner = interaction(site, treatment, corner, drop = TRUE),
    transect_id = interaction(site, treatment, corner, azi, drop = TRUE)
  )

woody_long <- paired |>
  select(site, treatment, corner, azi, block, design_corner, transect_id, woody_vegetation_prepct, woody_vegetation_postpct) |>
  pivot_longer(
    cols = starts_with("woody_vegetation_"),
    names_to = "phase",
    names_prefix = "woody_vegetation_",
    values_to = "load_mgha"
  ) |>
  mutate(phase = factor(phase, levels = c("prepct", "postpct")))

phase_formula <- load_mgha ~ phase * treatment +
  (1 | site) + (1 | block) + (1 | design_corner) + (1 | transect_id)

phase_registry <- bind_rows(
  fit_and_diagnose(
    "woody_phase_zigamma_zi1_disp1",
    "woody vegetation load",
    "matched repeated-measures phase model",
    woody_long,
    phase_formula,
    glmmTMB::ziGamma(link = "log"),
    "ziGamma(link = log)",
    ~1,
    "~1",
    ~1,
    "~1",
    paths
  ),
  fit_and_diagnose(
    "woody_phase_zigamma_ziphase_disp1",
    "woody vegetation load",
    "matched repeated-measures phase model",
    woody_long,
    phase_formula,
    glmmTMB::ziGamma(link = "log"),
    "ziGamma(link = log)",
    ~phase,
    "~phase",
    ~1,
    "~1",
    paths
  ),
  fit_and_diagnose(
    "woody_phase_zigamma_zi1_dispphase",
    "woody vegetation load",
    "matched repeated-measures phase model",
    woody_long,
    phase_formula,
    glmmTMB::ziGamma(link = "log"),
    "ziGamma(link = log)",
    ~1,
    "~1",
    ~phase,
    "~phase",
    paths
  )
)

delta_formula <- woody_standing_reduction_mgha ~ treatment +
  (1 | site) + (1 | block) + (1 | design_corner)

delta_registry <- bind_rows(
  fit_and_diagnose(
    "woody_delta_gaussian",
    "woody standing-vegetation reduction",
    "signed matched pre-minus-post contrast",
    paired,
    delta_formula,
    gaussian(),
    "Gaussian(identity)",
    ~0,
    "~0",
    ~1,
    "~1",
    paths
  ),
  fit_and_diagnose(
    "woody_delta_gaussian_disp_treatment",
    "woody standing-vegetation reduction",
    "signed matched pre-minus-post contrast",
    paired,
    delta_formula,
    gaussian(),
    "Gaussian(identity)",
    ~0,
    "~0",
    ~treatment,
    "~treatment",
    paths
  ),
  fit_and_diagnose(
    "woody_delta_student_t",
    "woody standing-vegetation reduction",
    "signed matched pre-minus-post contrast",
    paired,
    delta_formula,
    glmmTMB::t_family(),
    "Student-t(identity)",
    ~0,
    "~0",
    ~1,
    "~1",
    paths
  ),
  fit_and_diagnose(
    "woody_delta_student_t_disp_treatment",
    "woody standing-vegetation reduction",
    "signed matched pre-minus-post contrast",
    paired,
    delta_formula,
    glmmTMB::t_family(),
    "Student-t(identity)",
    ~0,
    "~0",
    ~treatment,
    "~treatment",
    paths
  ),
  fit_and_diagnose(
    "woody_delta_student_t_disp_treatment_site",
    "woody standing-vegetation reduction",
    "signed matched pre-minus-post contrast",
    paired,
    delta_formula,
    glmmTMB::t_family(),
    "Student-t(identity)",
    ~0,
    "~0",
    ~treatment + site,
    "~treatment + site",
    paths
  ),
  fit_and_diagnose(
    "woody_delta_student_t_disp_treatment_site_interaction",
    "woody standing-vegetation reduction",
    "signed matched pre-minus-post contrast",
    paired,
    delta_formula,
    glmmTMB::t_family(),
    "Student-t(identity)",
    ~0,
    "~0",
    ~treatment * site,
    "~treatment * site",
    paths
  )
)

write_csv(bind_rows(phase_registry, delta_registry), file.path(paths$manifests, "woody-pct-model-registry.csv"))
