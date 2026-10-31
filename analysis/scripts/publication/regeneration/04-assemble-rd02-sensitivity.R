# Assemble all fitted RD02 sensitivity alternatives; no fitting or inference.
# From analysis/: Rscript scripts/publication/regeneration/04-assemble-rd02-sensitivity.R
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else sys.frame(1)$ofile
script_dir <- dirname(normalizePath(script_file))
analysis_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
root <- file.path(analysis_root, "outputs/publication/regeneration")
joint_dir <- file.path(root, "random-effects-comparison/tables")
species_dir <- file.path(root, "separate-species-comparison/tables")
selected_dir <- file.path(root, "selected-species-models/tables")
inputs <- c(
  fit4 = file.path(joint_dir, "rd02-model-comparison.csv"),
  var4 = file.path(joint_dir, "rd02-random-effect-variances.csv"),
  diag4 = file.path(joint_dir, "rd02-dharma-tests.csv"),
  mean4 = file.path(joint_dir, "rd02-integrated-model-comparison.csv"),
  component2 = file.path(species_dir, "rd02-species-fit-register.csv"),
  fit2 = file.path(species_dir, "rd02-rwto-aic-comparison.csv"),
  var2 = file.path(species_dir, "rd02-species-variance-components.csv"),
  diag2 = file.path(species_dir, "rd02-species-dharma-tests.csv"),
  mean2 = file.path(species_dir, "rd02-species-integrated-estimates.csv"),
  selected_fit = file.path(selected_dir, "rd02-selected-species-fit-register.csv"),
  selected_var = file.path(selected_dir, "rd02-selected-species-variances.csv"),
  selected_diag = file.path(selected_dir, "rd02-selected-species-diagnostics.csv"),
  selected_mean = file.path(selected_dir, "rd02-selected-species-estimates.csv")
)
stopifnot(all(file.exists(inputs)))
hashes <- tools::md5sum(inputs)
read_input <- function(key) read.csv(inputs[[key]], stringsAsFactors = FALSE)
fit4 <- read_input("fit4")
component2 <- read_input("component2")
fit2 <- read_input("fit2")
selected_components <- read_input("selected_fit")
meta <- data.frame(
  model_id = c("thesis_site_species", "full_nested", "nested_plus_site_species",
               "joint_site_species_rwto", "joint_full_nested_rwto", "joint_combined_rwto",
               "separate_site_macro_pair", "selected_separate_four_species"),
  fit_scope = c(rep("Four-species fits", 3), rep("RW/TO fits", 4), "Four-species fits"),
  model_label = c("Site x species", "Full nesting", "Nested + site x species",
                  "Site x species", "Full nesting", "Nested + site x species", "Separate: site + macro plot",
                  "Separate: site + macro plot")
)
annotate <- function(x) {
  x$component_model_id <- x$model_id
  x$model_id[x$model_id %in% c("rw_site_macro", "to_site_macro")] <- "separate_site_macro_pair"
  stopifnot(all(x$model_id %in% meta$model_id))
  left_join(x, meta, by = "model_id")
}
annotate_selected <- function(x) {
  x$component_model_id <- if ("model_id" %in% names(x)) x$model_id else paste0(x$spp, "_site_macro")
  x$model_id <- "selected_separate_four_species"
  left_join(x, meta, by = "model_id")
}
pair_components <- component2[component2$model_id %in% c("rw_site_macro", "to_site_macro"), ]
fit2$convergence <- vapply(fit2$model_id, function(id) {
  rows <- if (id == "separate_site_macro_pair") pair_components else component2[component2$model_id == id, ]
  as.integer(any(rows$convergence != 0))
}, integer(1))
fit2$pd_hessian <- vapply(fit2$model_id, function(id) {
  rows <- if (id == "separate_site_macro_pair") pair_components else component2[component2$model_id == id, ]
  all(rows$pd_hessian)
}, logical(1))
fit <- bind_rows(
  select(fit4, model_id, n, k, log_likelihood, AIC, convergence, pd_hessian),
  select(fit2, model_id, n, k, log_likelihood, AIC, convergence, pd_hessian),
  data.frame(model_id = "selected_separate_four_species", n = sum(selected_components$n),
    k = sum(selected_components$k), log_likelihood = sum(selected_components$log_likelihood),
    AIC = sum(selected_components$AIC), convergence = as.integer(any(selected_components$convergence != 0)),
    pd_hessian = all(selected_components$pd_hessian))
) |>
  left_join(meta, by = "model_id") |>
  mutate(BIC = -2 * log_likelihood + log(n) * k,
         selected_primary = model_id == "selected_separate_four_species") |>
  group_by(fit_scope) |>
  mutate(delta_AIC_within_scope = AIC - min(AIC)) |>
  ungroup() |>
  arrange(match(model_id, c("thesis_site_species", "full_nested", "nested_plus_site_species",
                           "selected_separate_four_species", "joint_site_species_rwto",
                           "joint_full_nested_rwto", "joint_combined_rwto", "separate_site_macro_pair")))
variances <- bind_rows(annotate(read_input("var4")), annotate(read_input("var2")),
                      annotate_selected(read_input("selected_var"))) |>
  mutate(species_context = case_when(component_model_id == "rw_site_macro" ~ "Redwood",
                                     component_model_id == "to_site_macro" ~ "Tanoak",
                                     component_model_id == "df_site_macro" ~ "Douglas-fir",
                                     component_model_id == "other_site_macro" ~ "Other species",
                                     TRUE ~ "Shared variance"))
diagnostics <- bind_rows(
  annotate(filter(read_input("diag4"), scope == "species_plot_rows")),
  annotate(mutate(read_input("diag2"), scope = "species_plot_rows")),
  annotate_selected(mutate(read_input("selected_diag"), scope = "species_plot_rows"))
) |>
  mutate(species_context = case_when(component_model_id == "rw_site_macro" ~ "Redwood",
                                     component_model_id == "to_site_macro" ~ "Tanoak",
                                     component_model_id == "df_site_macro" ~ "Douglas-fir",
                                     component_model_id == "other_site_macro" ~ "Other species",
                                     fit_scope == "Four-species fits" ~ "All four species",
                                     TRUE ~ "RW + TO"))
mean_columns <- c("treat", "spp", "model_id", "estimate", "se", "lcl", "ucl",
                  "total_random_variance", "integration_multiplier")
means <- bind_rows(annotate(select(read_input("mean4"), all_of(mean_columns))),
                   annotate(select(read_input("mean2"), all_of(mean_columns))),
                   annotate_selected(select(read_input("selected_mean"), all_of(mean_columns))))
stopifnot(
  nrow(fit) == 8L, all(fit$convergence == 0), all(fit$pd_hessian),
  all(fit$n[fit$fit_scope == "Four-species fits"] == 256L),
  all(fit$n[fit$fit_scope == "RW/TO fits"] == 128L),
  max(abs(fit$AIC - (-2 * fit$log_likelihood + 2 * fit$k))) < 1e-8,
  max(abs(fit$BIC[match(fit4$model_id, fit$model_id)] - fit4$BIC)) < 1e-8,
  nrow(variances) == 28L, nrow(diagnostics) == 48L, nrow(means) == 96L,
  !anyDuplicated(means[, c("model_id", "spp", "treat")]),
  all(is.finite(means$estimate)), all(is.finite(means$se)),
  all(means$lcl <= means$estimate), all(means$ucl >= means$estimate)
)
# Verify that assembly preserves each source's numeric values exactly.
for (key in c("mean4", "mean2", "selected_mean")) {
  source_means <- read_input(key)
  assembled <- if (key == "selected_mean") filter(means, model_id == "selected_separate_four_species") else
    if (key == "mean4") filter(means, fit_scope == "Four-species fits", model_id != "selected_separate_four_species") else
      filter(means, fit_scope == "RW/TO fits")
  index <- match(paste(source_means$model_id, source_means$spp, source_means$treat),
                 paste(assembled$component_model_id, assembled$spp, assembled$treat))
  stopifnot(!anyNA(index), identical(source_means$estimate, assembled$estimate[index]),
            identical(source_means$lcl, assembled$lcl[index]), identical(source_means$ucl, assembled$ucl[index]))
}
out <- file.path(root, "overall-sensitivity")
for (directory in c("tables", "figures", "manifests")) {
  dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
}
tables <- list("rd02-overall-model-comparison.csv" = fit,
               "rd02-overall-variance-components.csv" = variances,
               "rd02-overall-dharma-tests.csv" = diagnostics,
               "rd02-overall-integrated-estimates.csv" = means)
for (filename in names(tables)) write.csv(tables[[filename]], file.path(out, "tables", filename), row.names = FALSE)
display <- filter(means, spp %in% c("rw", "to")) |>
  mutate(fit_scope = factor(fit_scope, levels = c("Four-species fits", "RW/TO fits")),
         model_label = factor(model_label, levels = unique(meta$model_label)),
         spp = factor(spp, levels = c("rw", "to")),
         treat = factor(treat, levels = c("gs", "ld", "ha", "hd")))
stopifnot(nrow(display) == 64L,
          all(count(display, fit_scope, model_label, spp)$n == 4L))
position <- position_dodge(width = .66)
figure <- ggplot(display, aes(treat, estimate, ymin = lcl, ymax = ucl,
                             color = model_label, shape = model_label, group = model_label)) +
  geom_linerange(linewidth = 1, position = position) +
  geom_point(size = 2.3, position = position) +
  facet_wrap(vars(fit_scope, spp), ncol = 2, scales = "free_y",
             labeller = labeller(spp = c(rw = "Redwood", to = "Tanoak"))) +
  scale_color_manual(values = c("#3F51B5", "#C96F38", "#237A57", "#8B4678")) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  scale_x_discrete(labels = toupper) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
  labs(x = "Retention treatment", y = expression(Basal~area~(m^2 %.% ha^-1)), color = NULL, shape = NULL) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE), shape = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold"),
        panel.spacing = grid::unit(1.2, "lines"))
ggsave(file.path(out, "figures/rd02-overall-sensitivity.png"), figure,
       width = 10, height = 8.5, dpi = 300, bg = "white")
ggsave(file.path(out, "figures/rd02-overall-sensitivity.pdf"), figure, width = 10, height = 8.5)
input_manifest <- data.frame(
  input = names(inputs), path = sub(paste0(analysis_root, "/"), "", unname(inputs), fixed = TRUE),
  md5 = unname(hashes))
write.csv(input_manifest, file.path(out, "manifests/rd02-overall-inputs.csv"), row.names = FALSE)
yaml::write_yaml(list(
  task = "RD02 overall sensitivity displays and tables", models_refitted = FALSE,
  script_md5 = unname(tools::md5sum(script_file)),
  inputs = "Generated outputs from scripts 02, 03, and 05; input hashes in rd02-overall-inputs.csv",
  configurations = 8L, estimate_rows = 96L, focal_figure_points = 64L,
  selected_primary = "selected_separate_four_species: user-selected species-specific site/macro-plot models",
  AIC = "Delta AIC calculated separately within the 256-row and 128-row data scopes",
  BIC = "-2 log likelihood + log(n) times parameter count; n=128 for the combined separate pair",
  separate_pair = "RW and TO components share a display label; component_model_id preserves their separate fits",
  estimates = "Original estimates and interval limits copied exactly; no inference recalculated",
  figure = "Two species columns; fitting-data scopes in rows; panel-specific vertical scales"
), file.path(out, "manifests/rd02-overall-provenance.yml"))
stopifnot(identical(unname(tools::md5sum(inputs)), unname(hashes)))
download_dir <- file.path(analysis_root, "../publication/assets/rd02-sensitivity")
for (directory in c("tables", "figures", "manifests")) {
  target <- file.path(download_dir, directory)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(list.files(file.path(out, directory), full.names = TRUE), target, overwrite = TRUE)))
}
stopifnot(file.copy(script_file, download_dir, overwrite = TRUE))
message("Assembled eight configurations, 96 unchanged mean/interval cells, and 64 focal figure points.")
