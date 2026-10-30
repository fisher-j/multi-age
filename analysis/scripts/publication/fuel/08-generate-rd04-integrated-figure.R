# Generate the R-D04 inventory with a consistent integrated mean-load estimand.
# Run from analysis/: Rscript scripts/publication/fuel/08-generate-rd04-integrated-figure.R
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
analysis_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
source(file.path(script_dir, "00-fuel-config.R"))
source(file.path(script_dir, "rd04-inference-utils.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glmmTMB)
  library(patchwork)
  library(readr)
})
paths <- publication_fuel_paths(analysis_root)
models_dir <- file.path(paths$output, "models")
figures_dir <- file.path(paths$output, "figures")
tables_dir <- file.path(paths$output, "tables")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

spec <- tibble::tribble(
  ~model_id, ~component, ~phase,
  "prepct_onehr_disp_treatment", "1-hour fuel", "prepct",
  "postpct_onehr_base", "1-hour fuel", "postpct",
  "prepct_tenhr_base", "10-hour fuel", "prepct",
  "postpct_tenhr_base", "10-hour fuel", "postpct",
  "prepct_hundhr_base", "100-hour fuel", "prepct",
  "postpct_hundhr_zi_treatment_site_disp_treatment_site_interaction", "100-hour fuel", "postpct",
  "prepct_thoushr_base", "1,000-hour fuel", "prepct",
  "postpct_thoushr_zi_treatment", "1,000-hour fuel", "postpct",
  "woody_phase_zigamma_ziphase_disp1", "Woody vegetation", NA_character_,
  "prepct_herbaceous_vegetation_base", "Herbaceous vegetation", "prepct",
  "postpct_herbaceous_vegetation_base", "Herbaceous vegetation", "postpct",
  "prepct_litter_disp_treatment", "Litter (pre-PCT only)", "prepct",
  "prepct_duff_base", "Duff (pre-PCT only)", "prepct"
)
selection <- read_csv(file.path(paths$manifests, "model-selection-log.csv"), show_col_types = FALSE)
stopifnot(all(spec$model_id %in% selection$provisional_aic_leader),
          all(file.exists(file.path(models_dir, paste0(spec$model_id, ".rds")))))

summarize_integrated <- function(s, component, model_id, phase = NULL, difference = FALSE) {
  prediction <- custom_integrated(s)
  # Means use log-delta intervals; signed differences use response-scale ones.
  low <- if (difference) prediction$estimate - qnorm(.975) * prediction$se else
    prediction$estimate * exp(-qnorm(.975) * prediction$se / prediction$estimate)
  high <- if (difference) prediction$estimate + qnorm(.975) * prediction$se else
    prediction$estimate * exp(qnorm(.975) * prediction$se / prediction$estimate)
  labels <- s$avg$labels
  phase_values <- if (difference) rep("Vegetation difference", nrow(labels)) else {
    values <- if ("phase" %in% names(labels)) as.character(labels$phase) else rep(phase, nrow(labels))
    stopifnot(all(values %in% c("prepct", "postpct")))
    ifelse(values == "prepct", "Pre-PCT", "Post-PCT")
  }
  tibble(component = component, phase = phase_values,
         treatment = as.character(labels$treatment), estimate = prediction$estimate,
         se = prediction$se, lcl = low, ucl = high, model_id = model_id,
         estimand = if (difference) "integrated matched woody mean difference (pre-PCT minus post-PCT)" else
           "site-standardized random-effect-integrated expected load",
         interval_method = if (difference) "response-scale delta method with joint phase covariance" else
           "log-scale delta method for integrated mean",
         random_intercept_variance = s$variance)
}

phase_estimates <- list()
for (i in seq_len(nrow(spec))) {
  item <- spec[i, ]
  model <- readRDS(file.path(models_dir, paste0(item$model_id, ".rds")))
  phase_arg <- if (is.na(item$phase)) NULL else item$phase
  s <- integration_setup(model, reference_grid(model, phase_arg))
  stopifnot(all(rowSums(s$avg$A > 0) == 4L))
  phase_estimates[[i]] <- summarize_integrated(s, item$component, item$model_id, phase_arg)
  if (item$component == "Woody vegetation") {
    difference_component <- summarize_integrated(
      woody_change_setup(s), "Woody vegetation difference (pre-PCT minus post-PCT)",
      item$model_id, difference = TRUE)
  }
}
estimates <- bind_rows(bind_rows(phase_estimates), difference_component) |>
  mutate(
    treatment = factor(treatment, levels = c("gs", "ld", "ha", "hd")),
    phase = factor(phase, levels = c("Pre-PCT", "Post-PCT", "Vegetation difference")),
    component = factor(component, levels = c(
      "1-hour fuel", "10-hour fuel", "100-hour fuel", "1,000-hour fuel",
      "Woody vegetation", "Herbaceous vegetation", "Litter (pre-PCT only)",
      "Duff (pre-PCT only)", "Woody vegetation difference (pre-PCT minus post-PCT)"))
  ) |>
  arrange(component, phase, treatment)
stopifnot(nrow(estimates) == 60L, all(is.finite(estimates$estimate)),
          all(is.finite(estimates$se)), all(estimates$se >= 0),
          all(estimates$lcl <= estimates$estimate), all(estimates$ucl >= estimates$estimate))
# Every displayed woody change must equal the plotted pre-minus-post means.
woody_means <- filter(estimates, component == "Woody vegetation")
for (trt in levels(estimates$treatment)) {
  pre <- filter(woody_means, treatment == trt, phase == "Pre-PCT")$estimate
  post <- filter(woody_means, treatment == trt, phase == "Post-PCT")$estimate
  delta <- filter(difference_component, treatment == trt)$estimate
  stopifnot(abs(pre - post - delta) < 1e-8)
}
write_csv(estimates, file.path(tables_dir, "rd04-fuel-model-estimates.csv"))
difference_component <- filter(estimates, phase == "Vegetation difference")

phase_colors <- c("Pre-PCT" = "#C96F38", "Post-PCT" = "#3F51B5")
phase_position <- position_dodge(width = 0.52)

phase_panel <- function(component_name) {
  data <- filter(estimates, .data$component == .env$component_name)
  ggplot(data, aes(x = treatment, y = estimate, ymin = lcl, ymax = ucl, color = phase, group = phase)) +
    geom_linerange(linewidth = 2.2, position = phase_position) +
    geom_point(shape = 21, fill = "black", color = "white", size = 2.5, stroke = 0.6, position = phase_position) +
    scale_color_manual(values = phase_colors, drop = FALSE) +
    scale_x_discrete(labels = c(gs = "GS", ld = "LD", ha = "HA", hd = "HD")) +
    labs(title = component_name, x = NULL, y = expression(Load ~ (Mg %.% ha^-1))) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
}

difference_panel <- function() {
  data <- difference_component
  ggplot(data, aes(x = treatment, y = estimate, ymin = lcl, ymax = ucl)) +
    geom_hline(yintercept = 0, color = "gray45", linewidth = 0.45) +
    geom_linerange(color = "#5A4A42", linewidth = 2.2) +
    geom_point(shape = 21, fill = "black", color = "white", size = 2.5, stroke = 0.6) +
    scale_x_discrete(labels = c(gs = "GS", ld = "LD", ha = "HA", hd = "HD")) +
    labs(
      title = "Woody vegetation difference\n(pre-PCT minus post-PCT)",
      x = NULL,
      y = expression(Load ~ difference ~ (Mg %.% ha^-1))
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11), panel.grid.minor = element_blank())
}

component_order <- levels(estimates$component)
plots <- lapply(component_order[1:8], phase_panel)
plots[[9]] <- difference_panel()
figure <- wrap_plots(plots, ncol = 3, guides = "collect") +
  plot_layout(guides = "collect")

ggsave(file.path(figures_dir, "rd04-fuel-phase-pct-inventory.png"), figure, width = 10.5, height = 10, dpi = 300, bg = "white")

# The analysis output above is canonical. A byte-identical site asset makes the
# current review figure available in the rendered Quarto website without asking
# the published site to reach outside its output directory.
site_assets_dir <- file.path(dirname(analysis_root), "publication", "assets")
dir.create(site_assets_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(
  file.path(figures_dir, "rd04-fuel-phase-pct-inventory.png"),
  file.path(site_assets_dir, "rd04-fuel-phase-pct-inventory.png"),
  overwrite = TRUE
)

provenance <- c(
  "task: R-D04",
  "generator: scripts/publication/fuel/08-generate-rd04-integrated-figure.R",
  "figure: figures/rd04-fuel-phase-pct-inventory.png",
  "estimates: tables/rd04-fuel-model-estimates.csv",
  "estimand: site-standardized random-effect-integrated mean load including modeled zeros",
  "random_effects: analytically integrated over all fitted independent Gaussian random intercepts",
  "site_averaging: equal weights for the four observed study-site contexts",
  "load_intervals: 95% log-scale delta-method confidence intervals for integrated means",
  "difference_intervals: 95% response-scale delta-method confidence intervals with joint pre/post covariance",
  "parameter_covariance: full sdr$cov.fixed including zero-process coefficients and log random-effect SDs",
  "difference_panel: integrated matched woody mean difference from the same repeated-phase model as woody load panels",
  "difference_model: woody_phase_zigamma_ziphase_disp1",
  "pre_pct_only: litter; duff",
  paste0("model_ids: ", paste(spec$model_id, collapse = "; ")),
  "selection_status: existing AIC-leading load models used for draft figure; scientific review remains pending"
)
writeLines(provenance, file.path(paths$manifests, "rd04-figure-provenance.yml"))
writeLines(c(provenance, "", capture.output(sessionInfo())),
           file.path(paths$manifests, "rd04-figure-session-info.txt"))

