# Read-only audit of the fitted RD04 models and thesis fuel predictions.
# Run from publication/: Rscript scripts/fuel/07-audit-rd04-inference.R
# Historical zero-effect estimates are read from the retired generator.
script_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot(length(script_argument) == 1L)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
suppressPackageStartupMessages({
  library(dplyr)
  library(glmmTMB)
  library(marginaleffects)
  library(readr)
  library(tidyr)
})
models_dir <- file.path(paths_common$output, "fuel/models")
audit_dir <- file.path(paths_common$output, "fuel/inference-audit")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

# Evaluate only these two definitions, never the original script's output steps.
original_script <- file.path(script_dir, "retired", "06-generate-rd04-figure.R")
original_hash <- tools::md5sum(original_script)
for (expr in parse(original_script)) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.symbol(expr[[2]]) &&
      as.character(expr[[2]]) %in% c("model_ids", "model_estimates")) eval(expr)
}

source(file.path(script_dir, "rd04-inference-utils.R"))

spec <- tibble(
  key = names(model_ids), model_id = unname(model_ids),
  component = c("1-hour", "1-hour", "10-hour", "10-hour", "100-hour", "100-hour",
                "1,000-hour", "1,000-hour", "Woody vegetation", "Herbaceous vegetation",
                "Herbaceous vegetation", "Litter", "Duff", "Woody difference"),
  phase = c(rep(c("prepct", "postpct"), 4), NA, "prepct", "postpct", "prepct", "prepct", NA)
)
spec <- bind_rows(filter(spec, key != "woody_phase"),
                  mutate(filter(spec, key == "woody_phase"), phase = "prepct"),
                  mutate(filter(spec, key == "woody_phase"), phase = "postpct"))

audit <- list()
structures <- list()
for (i in seq_len(nrow(spec))) {
  item <- spec[i, ]
  message("Auditing ", item$model_id, " ", item$phase)
  model <- readRDS(file.path(models_dir, paste0(item$model_id, ".rds")))
  phase_arg <- if (is.na(item$phase)) NULL else item$phase
  grid <- reference_grid(model, phase_arg)
  avg <- averaging_matrix(grid)
  reference <- custom_reference(model, grid)
  me <- as.data.frame(avg_predictions(model, newdata = grid, type = "response",
                       by = intersect(c("treatment", "phase"), names(grid)),
                       re.form = NA, allow.new.levels = TRUE))
  me <- me[match(as.character(reference$treatment), as.character(me$treatment)), ]
  legacy <- model_estimates(item$model_id, item$component, phase_arg,
                            difference = item$key == "woody_difference")
  legacy <- legacy[match(as.character(reference$treatment), as.character(legacy$treatment)), ]
  s <- integration_setup(model, grid)
  integrated <- custom_integrated(s)
  me_integrated <- as.data.frame(marginaleffects_integrated(s))
  stopifnot(max(abs(reference$estimate - me$estimate)) < 1e-6,
            max(abs(reference$std.error - me$std.error) / pmax(1, reference$std.error)) < 1e-4,
            max(abs(reference$estimate - legacy$estimate)) < 1e-6,
            max(abs(integrated$estimate - me_integrated$estimate)) < 1e-6,
            max(abs(integrated$se - me_integrated$std.error) / pmax(1, integrated$se)) < 1e-4)
  # Verify integration at a point using numerical quadrature, independently of
  # the closed-form exp(v/2) calculation (including near-zero variances).
  if (s$link == "log") {
    quadrature <- integrate(function(z) exp(sqrt(s$variance) * z) * dnorm(z),
                            lower = -12, upper = 12, rel.tol = 1e-9)$value
    stopifnot(abs(quadrature - exp(s$variance / 2)) < 1e-7)
  }
  # Check matrix construction against the model's own predict implementation.
  row0 <- if (s$link == "log") exp(drop(s$X %*% s$par[s$ib])) else drop(s$X %*% s$par[s$ib])
  if (length(s$iz)) row0 <- row0 * (1 - plogis(drop(s$Z %*% s$par[s$iz])))
  stopifnot(max(abs(row0 - predict(model, grid, type = "response", re.form = NA))) < 1e-6)
  # Average ALL observed rows within site, then equally across sites; retaining
  # only one corner/transect per site would give an arbitrary conditional mean.
  observed <- model$frame
  if (!is.null(phase_arg) && "phase" %in% names(observed)) observed <- filter(observed, .data$phase == .env$phase_arg)
  conditional <- observed |>
    mutate(predicted = as.numeric(predict(model, observed, type = "response", re.form = NULL))) |>
    group_by(across(all_of(intersect(c("site", "treatment", "phase"), names(observed))))) |>
    summarise(predicted = mean(predicted), .groups = "drop") |>
    group_by(across(all_of(intersect(c("treatment", "phase"), names(observed))))) |>
    summarise(predicted = mean(predicted), .groups = "drop")
  fitted_groups <- conditional$predicted[match(as.character(reference$treatment), as.character(conditional$treatment))]
  # Marginal mean intervals: log-delta intervals stay positive without truncation;
  # the signed identity-link difference uses ordinary response-scale intervals.
  lcl <- if (s$link == "log") integrated$estimate * exp(-qnorm(.975) * integrated$se / integrated$estimate) else integrated$estimate - qnorm(.975) * integrated$se
  ucl <- if (s$link == "log") integrated$estimate * exp(qnorm(.975) * integrated$se / integrated$estimate) else integrated$estimate + qnorm(.975) * integrated$se
  audit[[i]] <- tibble(
    model_id = item$model_id, component = item$component, phase = item$phase,
    treatment = as.character(reference$treatment), n_sites = rowSums(avg$A > 0),
    reference_custom = reference$estimate, reference_marginaleffects = me$estimate,
    original_se = legacy$se, corrected_reference_se = reference$std.error,
    marginaleffects_reference_se = me$std.error,
    fitted_group_site_balanced_mean = fitted_groups,
    random_intercept_variance = s$variance,
    integration_multiplier = if (s$link == "log") exp(s$variance / 2) else 1,
    integrated_custom = integrated$estimate,
    integrated_marginaleffects = me_integrated$estimate,
    integrated_se_custom = integrated$se,
    integrated_se_marginaleffects = me_integrated$std.error,
    integrated_lcl = lcl, integrated_ucl = ucl
  )
  structures[[i]] <- tibble(model_id = item$model_id, phase = item$phase,
                            conditional = deparse1(formula(model)),
                            zero = deparse1(formula(model, component = "zi")),
                            dispersion = deparse1(formula(model, component = "disp")))
}
audit <- bind_rows(audit)
write_csv(audit, file.path(audit_dir, "rd04-estimand-comparison.csv"))
structure_path <- file.path(audit_dir, "model-structures.csv")
structure_table <- as.data.frame(bind_rows(structures))
# Preserve an unchanged model inventory, including when it is open for review.
if (!file.exists(structure_path) ||
    !identical(structure_table, as.data.frame(read_csv(structure_path, show_col_types = FALSE)))) {
  write_csv(structure_table, structure_path)
}

# A coherent mean change uses the same repeated-phase model on both sides and
# its cross-phase covariance. Compare with the independently fitted t model.
woody <- readRDS(file.path(models_dir, paste0(model_ids[["woody_phase"]], ".rds")))
s <- integration_setup(woody, reference_grid(woody))
s <- woody_change_setup(s)
treatments <- as.character(s$avg$labels$treatment)
change <- custom_integrated(s)
change_me <- as.data.frame(marginaleffects_integrated(s))
stopifnot(max(abs(change$estimate - change_me$estimate)) < 1e-6,
          max(abs(change$se - change_me$std.error) / pmax(1, change$se)) < 1e-4)
observed <- woody$frame |>
  group_by(treatment, phase, site) |>
  summarise(load = mean(load_mgha), .groups = "drop") |>
  group_by(treatment, phase) |>
  summarise(load = mean(load), .groups = "drop") |>
  pivot_wider(names_from = phase, values_from = load) |>
  mutate(observed_difference = prepct - postpct)
phase_change <- tibble(
  treatment = treatments, phase_model_difference = change$estimate,
  phase_model_difference_marginaleffects = change_me$estimate,
  se_custom = change$se, se_marginaleffects = change_me$std.error,
  lcl = change$estimate - qnorm(.975) * change$se,
  ucl = change$estimate + qnorm(.975) * change$se
) |>
  left_join(select(observed, treatment, observed_difference), by = "treatment") |>
  left_join(select(filter(audit, component == "Woody difference"), treatment,
                   separate_t_model_difference = integrated_custom), by = "treatment")
write_csv(phase_change, file.path(audit_dir, "woody-mean-change-comparison.csv"))

# Reproduce the thesis RESULTS grid on the original saved models, without
# executing or modifying any thesis notebook or changing its fitted models.
# Historical predictions were replayed and verified in the thesis environment.
thesis_reference <- file.path(paths_common$input, "fuel/thesis-reference-reproduction.csv")
thesis_results <- read_csv(thesis_reference, show_col_types = FALSE)
stopifnot(file.copy(thesis_reference,
                   file.path(audit_dir, "thesis-reference-reproduction.csv"), overwrite = TRUE))
lookup <- c("1-hour" = "onehr", "10-hour" = "tenhr", "100-hour" = "hundhr", "1,000-hour" = "thoushr")
comparison <- audit |>
  filter(component %in% names(lookup)) |>
  mutate(thesis_class = unname(lookup[component])) |>
  left_join(select(thesis_results, phase, thesis_class, treatment, thesis_reference, thesis_se),
            by = c("phase", "thesis_class", "treatment")) |>
  mutate(reference_change = reference_custom - thesis_reference)
write_csv(comparison, file.path(audit_dir, "thesis-rd04-deadwood-comparison.csv"))

stopifnot(identical(original_hash, tools::md5sum(original_script)))
writeLines(c("RD04 inference audit; existing fitted models; no refitting or model selection.",
             paste("Original script MD5:", unname(original_hash)),
             "Integrated SEs include beta, betazi, and log-SD covariance via sdr$cov.fixed.",
             "Intervals describe estimated means, not new-site or new-observation predictions.",
             capture.output(sessionInfo())), file.path(audit_dir, "session-info.txt"))
print(select(audit, component, phase, treatment, reference_custom, integrated_custom,
             original_se, corrected_reference_se), n = Inf)
