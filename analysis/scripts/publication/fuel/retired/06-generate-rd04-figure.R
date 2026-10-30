# Generate the R-D04 publication figure: the full disaggregated pre-/post-PCT
# fuel inventory. This publication-only step reads saved F-01 model artifacts
# and writes only below outputs/publication/fuel/.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) stop("Run with Rscript scripts/publication/fuel/06-generate-rd04-figure.R")
script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
analysis_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
source(file.path(script_dir, "00-fuel-config.R"))

library(dplyr)
library(ggplot2)
library(glmmTMB)
library(patchwork)
library(readr)
library(tidyr)

paths <- publication_fuel_paths(analysis_root)
output_root <- paths$output
models_dir <- file.path(output_root, "models")
figures_dir <- file.path(output_root, "figures")
tables_dir <- file.path(output_root, "tables")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

selection <- read_csv(file.path(paths$manifests, "model-selection-log.csv"), show_col_types = FALSE)

model_ids <- c(
  prepct_onehr = "prepct_onehr_disp_treatment",
  postpct_onehr = "postpct_onehr_base",
  prepct_tenhr = "prepct_tenhr_base",
  postpct_tenhr = "postpct_tenhr_base",
  prepct_hundhr = "prepct_hundhr_base",
  postpct_hundhr = "postpct_hundhr_zi_treatment_site_disp_treatment_site_interaction",
  prepct_thoushr = "prepct_thoushr_base",
  postpct_thoushr = "postpct_thoushr_zi_treatment",
  woody_phase = "woody_phase_zigamma_ziphase_disp1",
  prepct_herbaceous = "prepct_herbaceous_vegetation_base",
  postpct_herbaceous = "postpct_herbaceous_vegetation_base",
  prepct_litter = "prepct_litter_disp_treatment",
  prepct_duff = "prepct_duff_base",
  woody_difference = "woody_delta_student_t_disp_treatment_site_interaction"
)

missing_models <- model_ids[!file.exists(file.path(models_dir, paste0(model_ids, ".rds")))]
if (length(missing_models)) stop("Missing selected model files: ", paste(missing_models, collapse = ", "))

# Predict at every observed site-by-treatment (and phase, where relevant)
# combination, excluding random effects. Average the balanced site predictions
# within each plotted treatment/phase and propagate their prediction covariance
# to retain model-based uncertainty for the displayed marginal estimate.
model_estimates <- function(model_id, component, phase = NULL, difference = FALSE) {
  model <- readRDS(file.path(models_dir, paste0(model_id, ".rds")))
  phase_label <- if (difference) {
    "Vegetation difference"
  } else if (identical(phase, "prepct")) {
    "Pre-PCT"
  } else {
    "Post-PCT"
  }
  frame <- model$frame
  focal <- c("treatment", if ("phase" %in% names(frame)) "phase")
  grid_columns <- intersect(c("site", focal), names(frame))
  grid <- frame |>
    distinct(across(all_of(grid_columns)), .keep_all = TRUE)
  if (!is.null(phase) && "phase" %in% names(grid)) {
    grid <- filter(grid, .data$phase == .env$phase)
  }
  prediction <- predict(
    model,
    newdata = grid,
    type = "response",
    se.fit = TRUE,
    cov.fit = TRUE,
    re.form = NA,
    allow.new.levels = TRUE
  )
  grid <- mutate(grid, estimate_site = as.numeric(prediction$fit))
  group_columns <- intersect(c("treatment", "phase"), names(grid))
  groups <- grid |>
    mutate(row_id = row_number()) |>
    group_by(across(all_of(group_columns))) |>
    summarise(row_ids = list(row_id), estimate = mean(estimate_site), .groups = "drop")
  groups <- groups |>
    rowwise() |>
    mutate(
      se = sqrt(sum(prediction$cov.fit[row_ids[[1]], row_ids[[1]], drop = FALSE]) / length(row_ids[[1]])^2),
      lcl = estimate - stats::qnorm(0.975) * se,
      ucl = estimate + stats::qnorm(0.975) * se
    ) |>
    ungroup() |>
    mutate(
      component = component,
      model_id = model_id,
      estimand = if (difference) "matched woody vegetation difference (pre-PCT minus post-PCT)" else "phase-specific expected load",
      phase = phase_label,
      lcl = if (difference) lcl else pmax(0, lcl),
      treatment = factor(treatment, levels = c("gs", "ld", "ha", "hd"))
    )
  select(groups, component, phase, treatment, estimate, se, lcl, ucl, model_id, estimand)
}

paired_components <- bind_rows(
  model_estimates(model_ids[["prepct_onehr"]], "1-hour fuel", "prepct"),
  model_estimates(model_ids[["postpct_onehr"]], "1-hour fuel", "postpct"),
  model_estimates(model_ids[["prepct_tenhr"]], "10-hour fuel", "prepct"),
  model_estimates(model_ids[["postpct_tenhr"]], "10-hour fuel", "postpct"),
  model_estimates(model_ids[["prepct_hundhr"]], "100-hour fuel", "prepct"),
  model_estimates(model_ids[["postpct_hundhr"]], "100-hour fuel", "postpct"),
  model_estimates(model_ids[["prepct_thoushr"]], "1,000-hour fuel", "prepct"),
  model_estimates(model_ids[["postpct_thoushr"]], "1,000-hour fuel", "postpct"),
  model_estimates(model_ids[["woody_phase"]], "Woody vegetation", "prepct"),
  model_estimates(model_ids[["woody_phase"]], "Woody vegetation", "postpct"),
  model_estimates(model_ids[["prepct_herbaceous"]], "Herbaceous vegetation", "prepct"),
  model_estimates(model_ids[["postpct_herbaceous"]], "Herbaceous vegetation", "postpct")
)

pre_only_components <- bind_rows(
  model_estimates(model_ids[["prepct_litter"]], "Litter (pre-PCT only)", "prepct"),
  model_estimates(model_ids[["prepct_duff"]], "Duff (pre-PCT only)", "prepct")
)

difference_component <- model_estimates(
  model_ids[["woody_difference"]],
  "Woody vegetation difference (pre-PCT minus post-PCT)",
  difference = TRUE
)

estimates <- bind_rows(paired_components, pre_only_components, difference_component) |>
  mutate(
    phase = factor(phase, levels = c("Pre-PCT", "Post-PCT", "Vegetation difference")),
    component = factor(component, levels = c(
    "1-hour fuel", "10-hour fuel", "100-hour fuel", "1,000-hour fuel",
    "Woody vegetation", "Herbaceous vegetation", "Litter (pre-PCT only)",
    "Duff (pre-PCT only)", "Woody vegetation difference (pre-PCT minus post-PCT)"
    ))
  )

write_csv(estimates, file.path(tables_dir, "rd04-fuel-model-estimates.csv"))

phase_colors <- c("Pre-PCT" = "#C96F38", "Post-PCT" = "#3F51B5")
phase_position <- position_dodge(width = 0.52)

phase_panel <- function(component_name) {
  data <- filter(estimates, .data$component == .env$component_name)
  ggplot(data, aes(x = treatment, y = estimate, ymin = lcl, ymax = ucl, color = phase, group = phase)) +
    geom_linerange(linewidth = 2.2, position = phase_position) +
    geom_point(shape = 21, fill = "black", size = 2.5, stroke = 0.6, position = phase_position) +
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
    geom_point(shape = 21, fill = "black", size = 2.5, stroke = 0.6) +
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
  "figure: figures/rd04-fuel-phase-pct-inventory.png",
  "estimates: tables/rd04-fuel-model-estimates.csv",
  "intervals: 95% normal-approximation intervals propagated from prediction covariance",
  "site_averaging: balanced average of population-level predictions across observed sites",
  "paired_components: 1-hour; 10-hour; 100-hour; 1,000-hour; woody vegetation; herbaceous vegetation",
  "pre_pct_only: litter; duff",
  "difference_panel: matched woody vegetation pre-PCT minus post-PCT",
  paste0("model_ids: ", paste(model_ids, collapse = "; ")),
  "selection_status: AIC-leading candidates used for draft figure; scientific review remains pending"
)
writeLines(provenance, file.path(paths$manifests, "rd04-figure-provenance.yml"))
