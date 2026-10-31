# Compare the thesis covariance structure with the full sampling hierarchy.
# From analysis/: Rscript scripts/publication/regeneration/02-compare-rd02-random-effects.R
# Also sourceable; preserves the saved thesis fit and the current lead display.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else sys.frame(1)$ofile
script_dir <- dirname(normalizePath(script_file))
analysis_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
suppressPackageStartupMessages({
  library(glmmTMB)
  library(dplyr)
  library(ggplot2)
  library(DHARMa)
})
input <- file.path(analysis_root, "regen_visualize.rda")
input_hash <- tools::md5sum(input)
lead_files <- file.path(analysis_root, c(
  "outputs/publication/regeneration/figures/rd02-regeneration-basal-area.png",
  "outputs/publication/regeneration/tables/rd02-basal-area-model-estimates.csv"))
lead_hashes <- tools::md5sum(lead_files)
saved <- new.env()
load(input, envir = saved)
d <- as.data.frame(saved$regen_pa)
d$site <- factor(d$site)
d$plot <- factor(d$plot)
stopifnot(nrow(d) == 256L, !anyNA(d),
          nrow(distinct(d, site, treat, plot)) == 64L,
          all(count(d, site, treat, plot)$n == 4L),
          all(count(d, site, treat, spp)$n == 4L),
          isTRUE(all.equal(d$ba_ha, unname(model.response(model.frame(saved$mba))))))
out <- file.path(analysis_root, "outputs/publication/regeneration/random-effects-comparison")
for (directory in c("models", "diagnostics", "tables", "figures", "manifests")) {
  dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
}
formulas <- list(
  thesis_site_species = ba_ha ~ treat * spp + (1 | site:spp),
  full_nested = ba_ha ~ treat * spp + (1 | site/treat/plot),
  nested_plus_site_species = ba_ha ~ treat * spp + (1 | site/treat/plot) + (1 | site:spp)
)
models <- list()
records <- list()
variance_records <- list()
diagnostic_records <- list()
seed <- 20261030L
simulations <- 1000L

test_record <- function(sim, id, scope) {
  tests <- list(uniformity = DHARMa::testUniformity,
                dispersion = DHARMa::testDispersion,
                outliers = DHARMa::testOutliers,
                zero_inflation = DHARMa::testZeroInflation)
  bind_rows(lapply(names(tests), function(name) {
    result <- tests[[name]](sim, plot = FALSE)
    data.frame(model_id = id, scope = scope, test = name,
               statistic = unname(result$statistic)[1], p_value = result$p.value)
  }))
}

for (id in names(formulas)) {
  message("Fitting ", id)
  fit_warnings <- character()
  fit <- withCallingHandlers(
    glmmTMB(formulas[[id]], data = d, family = ziGamma(link = "log"),
            ziformula = ~spp, dispformula = ~spp,
            control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  models[[id]] <- fit
  saveRDS(fit, file.path(out, "models", paste0(id, ".rds")))
  vc <- VarCorr(fit)$cond
  variance_records[[id]] <- bind_rows(lapply(names(vc), function(group) {
    data.frame(model_id = id, group = group,
      n_groups = length(unique(model.frame(fit)[["site"]])),
      variance = vc[[group]][1, 1], sd = sqrt(vc[[group]][1, 1]))
  }))
  # Count actual unique design groups, rather than the reused plot/corner codes.
  variance_records[[id]]$n_groups <- vapply(variance_records[[id]]$group, function(g) {
    vars <- strsplit(g, ":", fixed = TRUE)[[1]]
    nrow(unique(d[vars]))
  }, integer(1))
  ll <- logLik(fit)
  records[[id]] <- data.frame(
    model_id = id, formula = paste(deparse(formulas[[id]]), collapse = " "),
    n = nobs(fit), k = attr(ll, "df"), log_likelihood = as.numeric(ll),
    AIC = AIC(fit), BIC = BIC(fit), convergence = fit$fit$convergence,
    pd_hessian = fit$sdr$pdHess,
    max_abs_gradient = max(abs(fit$sdr$gradient.fixed)),
    min_random_sd = min(variance_records[[id]]$sd),
    near_boundary_sd_lt_1e_3 = any(variance_records[[id]]$sd < 1e-3),
    warnings = paste(unique(fit_warnings), collapse = " | ")
  )
  # glmmTMB's simulation in this installed version draws new random effects.
  # Explicitly record the version and use the same seed and n for all models.
  sim <- simulateResiduals(fit, n = simulations, seed = seed, refit = FALSE, plot = FALSE)
  saveRDS(sim, file.path(out, "models", paste0(id, "-dharma.rds")))
  diagnostic_records[[id]] <- test_record(sim, id, "species_plot_rows")
  # Recalculate residuals on aggregated responses to inspect each design level.
  groups <- list(site = d$site, macro_plot = interaction(d$site, d$treat, drop = TRUE),
                 vegetation_plot = interaction(d$site, d$treat, d$plot, drop = TRUE))
  for (scope in names(groups)) {
    grouped <- recalculateResiduals(sim, group = groups[[scope]], seed = seed)
    diagnostic_records[[id]] <- bind_rows(diagnostic_records[[id]], test_record(grouped, id, scope))
  }
  png(file.path(out, "diagnostics", paste0(id, "-dharma.png")),
      width = 1800, height = 1000, res = 160)
  plot(sim)
  dev.off()
  png(file.path(out, "diagnostics", paste0(id, "-by-species-treatment.png")),
      width = 1800, height = 1000, res = 160)
  old_par <- par(mfrow = c(1, 2))
  plotResiduals(sim, form = d$spp)
  plotResiduals(sim, form = d$treat)
  par(old_par)
  dev.off()
}
comparison <- bind_rows(records) |>
  mutate(delta_AIC = AIC - min(AIC[convergence == 0 & pd_hessian], na.rm = TRUE))
variance_table <- bind_rows(variance_records)
diagnostics <- bind_rows(diagnostic_records)
stopifnot(abs(as.numeric(logLik(models$thesis_site_species)) -
                as.numeric(logLik(saved$mba))) < 1e-5)
# Boundary components warrant an optimizer/start sensitivity check. Start each
# random SD at 0.3 rather than inheriting the near-zero nlminb estimates.
optimizer_checks <- bind_rows(lapply(c("full_nested", "nested_plus_site_species"), function(id) {
  message("Checking BFGS optimizer/start sensitivity: ", id)
  warnings <- character()
  check <- withCallingHandlers(update(models[[id]],
    start = list(theta = rep(log(.3), sum(names(models[[id]]$fit$par) == "theta"))),
    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"),
                            optCtrl = list(maxit = 10000, reltol = 1e-10))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  saveRDS(check, file.path(out, "models", paste0(id, "-bfgs.rds")))
  difference <- check$fit$objective - models[[id]]$fit$objective
  stopifnot(check$fit$convergence == 0, abs(difference) < 1e-4)
  data.frame(model_id = id, optimizer = "BFGS; initial random SDs 0.3",
    convergence = check$fit$convergence, pd_hessian = check$sdr$pdHess,
    objective_difference_from_nlminb = difference,
    warnings = paste(unique(warnings), collapse = " | "))
}))
write.csv(optimizer_checks, file.path(out, "tables/rd02-optimizer-sensitivity.csv"), row.names = FALSE)

# Inspect the baseline tail flag without removing observations or changing the
# fit. More simulations and a second seed check Monte Carlo sensitivity.
tail_check <- simulateResiduals(models$thesis_site_species, n = 10000,
                                seed = seed + 1L, refit = FALSE, plot = FALSE)
tail_tests <- test_record(tail_check, "thesis_site_species", "species_plot_rows_10000_simulations")
write.csv(tail_tests, file.path(out, "tables/rd02-baseline-tail-sensitivity.csv"), row.names = FALSE)
tail_rows <- bind_rows(lapply(c(1000L, 10000L), function(n) {
  residuals <- if (n == 1000L) readRDS(file.path(out, "models/thesis_site_species-dharma.rds"))$scaledResiduals else
    tail_check$scaledResiduals
  d |>
    mutate(row_id = seq_len(nrow(d)), n_simulations = n, dharma_residual = residuals) |>
    filter(dharma_residual < 1 / (n + 1) | dharma_residual > 1 - 1 / (n + 1))
}))
write.csv(tail_rows, file.path(out, "tables/rd02-baseline-tail-observations.csv"), row.names = FALSE)
write.csv(comparison, file.path(out, "tables/rd02-model-comparison.csv"), row.names = FALSE)
write.csv(variance_table, file.path(out, "tables/rd02-random-effect-variances.csv"), row.names = FALSE)
write.csv(diagnostics, file.path(out, "tables/rd02-dharma-tests.csv"), row.names = FALSE)

# Integrate each candidate over ALL of its independent conditional random
# intercepts. Zero and dispersion specifications are identical across models.
integrated_estimates <- function(m, id) {
  stopifnot(m$fit$convergence == 0, m$sdr$pdHess,
            family(m)$link == "log", length(VarCorr(m)$zi) == 0L,
            all(vapply(VarCorr(m)$cond, function(v) identical(dim(v), c(1L, 1L)), logical(1))))
  grid <- distinct(model.frame(m), site, treat, spp, .keep_all = TRUE)
  labels <- distinct(grid, treat, spp)
  A <- t(vapply(seq_len(nrow(labels)), function(i) {
    keep <- grid$treat == labels$treat[i] & grid$spp == labels$spp[i]
    as.numeric(keep) / sum(keep)
  }, numeric(nrow(grid))))
  stopifnot(all(rowSums(A > 0) == 4L))
  X <- model.matrix(delete.response(terms(formula(m, fixed.only = TRUE))), grid)
  Z <- model.matrix(formula(m, component = "zi", fixed.only = TRUE), grid)
  b <- m$fit$par
  ib <- which(names(b) == "beta")
  iz <- which(names(b) == "betazi")
  it <- which(names(b) == "theta")
  stopifnot(identical(colnames(X), names(fixef(m)$cond)),
            identical(colnames(Z), names(fixef(m)$zi)),
            length(it) == length(VarCorr(m)$cond),
            isTRUE(all.equal(unname(b), unname(m$sdr$par.fixed))))
  variance <- sum(exp(2 * b[it]))
  pzero <- plogis(drop(Z %*% b[iz]))
  mu <- exp(drop(X %*% b[ib]) + variance / 2) * (1 - pzero)
  D <- matrix(0, nrow(grid), length(b))
  D[, ib] <- mu * X
  D[, iz] <- -mu * pzero * Z
  D[, it] <- outer(mu, exp(2 * b[it]))
  G <- A %*% D
  est <- drop(A %*% mu)
  se <- sqrt(diag(G %*% m$sdr$cov.fixed %*% t(G)))
  functional <- function(b) drop(A %*% (
    exp(drop(X %*% b[ib]) + sum(exp(2 * b[it])) / 2) *
      (1 - plogis(drop(Z %*% b[iz])))))
  numeric_G <- numDeriv::jacobian(functional, unname(b))
  stopifnot(max(abs(G - numeric_G) / pmax(1, abs(G))) < 1e-5,
            max(abs(mu / exp(variance / 2) -
                      predict(m, grid, type = "response", re.form = NA))) < 1e-7,
            abs(sum(vapply(VarCorr(m)$cond, function(v) v[1, 1], numeric(1))) - variance) < 1e-9)
  mutate(labels, model_id = id, estimate = est, se = se,
    lcl = est * exp(-qnorm(.975) * se / est), ucl = est * exp(qnorm(.975) * se / est),
    total_random_variance = variance, integration_multiplier = exp(variance / 2))
}
eligible <- comparison$model_id[comparison$convergence == 0 & comparison$pd_hessian]
estimates <- bind_rows(lapply(eligible, function(id) integrated_estimates(models[[id]], id)))
baseline <- filter(estimates, model_id == "thesis_site_species") |>
  select(treat, spp, baseline_mean = estimate, baseline_lcl = lcl, baseline_ucl = ucl)
estimates <- left_join(estimates, baseline, by = c("treat", "spp")) |>
  mutate(mean_percent_change = 100 * (estimate / baseline_mean - 1),
         ci_width_ratio = (ucl - lcl) / (baseline_ucl - baseline_lcl))
write.csv(estimates, file.path(out, "tables/rd02-integrated-model-comparison.csv"), row.names = FALSE)

model_labels <- c(thesis_site_species = "Thesis: site x species",
                  full_nested = "Full nested design",
                  nested_plus_site_species = "Nested + site x species")
display <- estimates |>
  filter(spp %in% c("rw", "to")) |>
  mutate(model_id = factor(model_id, levels = names(formulas)),
         spp = factor(spp, levels = c("rw", "to")))
position <- position_dodge(width = .58)
figure <- ggplot(display, aes(treat, estimate, ymin = lcl, ymax = ucl,
                             color = model_id, shape = model_id, group = model_id)) +
  geom_linerange(linewidth = 1, position = position) +
  geom_point(size = 2.4, position = position) +
  facet_wrap(~spp, nrow = 1, scales = "free_y",
             labeller = as_labeller(c(rw = "Redwood", to = "Tanoak"))) +
  scale_color_manual(values = c("#3F51B5", "#C96F38", "#237A57"), labels = model_labels) +
  scale_shape_manual(values = c(16, 17, 15), labels = model_labels) +
  scale_x_discrete(labels = toupper) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
  labs(x = "Retention treatment", y = expression(Basal~area~(m^2 %.% ha^-1)),
       color = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold"))
figure_file <- file.path(out, "figures/rd02-random-effects-comparison.png")
ggsave(figure_file, figure, width = 9, height = 4.8, dpi = 300, bg = "white")
ggsave(sub("png$", "pdf", figure_file), figure, width = 9, height = 4.8)
provenance <- list(
  task = "RD02 random-effects comparison", source = "analysis/regen_visualize.rda",
  source_md5 = unname(input_hash), script_md5 = unname(tools::md5sum(script_file)),
  data = "same 256 species-plot rows; 4 sites, 16 treatment macro plots, 64 vegetation plots",
  candidate_formulas = lapply(formulas, function(f) paste(deparse(f), collapse = " ")),
  fixed_components = "ziGamma(log); conditional treat*spp; zero ~spp; dispersion ~spp",
  fit = "maximum likelihood; nlminb; iter.max=eval.max=10000",
  diagnostics = list(seed = seed, simulations = simulations, refit = FALSE,
                     random_effect_simulation = "unconditional glmmTMB simulate default in installed version",
                     baseline_tail_sensitivity = "10000 simulations; seed 20261031"),
  optimizer_sensitivity = "BFGS from random SDs 0.3, objective checked within 1e-4 of nlminb",
  means = "exact integration over all conditional Gaussian random intercepts, including zeros",
  intervals = "95% log-scale delta method with full parameter covariance",
  tests = "AIC comparison; primary covariance structures are not nested in one another, so no ordinary LRT",
  selection = "comparison only; original lead figure and model not replaced"
)
yaml::write_yaml(provenance, file.path(out, "manifests/rd02-comparison-provenance.yml"))
capture.output(sessionInfo(), file = file.path(out, "manifests/rd02-comparison-session-info.txt"))
stopifnot(identical(unname(tools::md5sum(input)), unname(input_hash)),
          identical(unname(tools::md5sum(lead_files)), unname(lead_hashes)))
# Publish only review artifacts; fitted models and simulations stay in the
# analysis output directory (RDS files follow the repository's ignore policy).
download_dir <- file.path(analysis_root, "../publication/assets/rd02-comparison")
for (directory in c("tables", "figures", "diagnostics", "manifests")) {
  target <- file.path(download_dir, directory)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(list.files(file.path(out, directory), full.names = TRUE),
                         target, overwrite = TRUE)))
}
stopifnot(all(file.copy(c(script_file, file.path(script_dir, "README.md")),
                       download_dir, overwrite = TRUE)))
message("Comparison complete; original thesis input unchanged.")
print(comparison)
print(variance_table)
print(filter(diagnostics, scope == "species_plot_rows"))
print(as.data.frame(select(display, model_id, spp, treat, estimate, lcl, ucl, mean_percent_change)))
