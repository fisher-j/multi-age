# RD02: separate RW/TO models with site and nested macro-plot intercepts.
# From publication/: Rscript scripts/regeneration/03-compare-rd02-separate-species.R
# Also sourceable. Original thesis and current lead display are preserved.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else sys.frame(1)$ofile
script_dir <- dirname(normalizePath(script_file))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
suppressPackageStartupMessages({
  library(glmmTMB)
  library(DHARMa)
  library(dplyr)
  library(ggplot2)
})
input <- file.path(paths_common$input, "regeneration-measurements.rds")
protected <- c(input, file.path(paths_common$output, c(
  "regeneration/figures/rd02-regeneration-basal-area.png",
  "regeneration/tables/rd02-basal-area-model-estimates.csv")))
protected_hashes <- tools::md5sum(protected)
saved <- list(regen_pa = readRDS(input))
d <- droplevels(subset(as.data.frame(saved$regen_pa), spp %in% c("rw", "to")))
d$site <- factor(d$site)
d$plot <- factor(d$plot)
stopifnot(nrow(d) == 128L, !anyNA(d),
          nrow(distinct(d, site, treat, plot)) == 64L,
          all(count(d, site, treat, plot)$n == 2L))
out <- file.path(paths_common$output, "regeneration/separate-species-comparison")
for (directory in c("models", "tables", "diagnostics", "figures", "manifests")) {
  dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
}
spec <- list(
  rw_site_macro = list(species = "rw", formula = ba_ha ~ treat + (1 | site/treat)),
  to_site_macro = list(species = "to", formula = ba_ha ~ treat + (1 | site/treat)),
  joint_site_species_rwto = list(species = NULL, formula = ba_ha ~ treat*spp + (1 | site:spp)),
  joint_full_nested_rwto = list(species = NULL, formula = ba_ha ~ treat*spp + (1 | site/treat/plot)),
  joint_combined_rwto = list(species = NULL, formula = ba_ha ~ treat*spp + (1 | site/treat/plot) + (1 | site:spp))
)
models <- list()
fit_records <- variance_records <- diagnostic_records <- list()
seed <- 20261030L
for (id in names(spec)) {
  item <- spec[[id]]
  model_data <- if (is.null(item$species)) d else droplevels(d[d$spp == item$species, ])
  if (!is.null(item$species)) {
    stopifnot(nrow(model_data) == 64L, all(count(model_data, site, treat, plot)$n == 1L))
  }
  message("Fitting ", id)
  warnings <- character()
  m <- withCallingHandlers(
    glmmTMB(item$formula, data = model_data, family = ziGamma(link = "log"),
            ziformula = if (is.null(item$species)) ~spp else ~1,
            dispformula = if (is.null(item$species)) ~spp else ~1,
            control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  models[[id]] <- m
  if (!is.null(item$species)) {
    stopifnot(setequal(names(VarCorr(m)$cond), c("site", "treat:site")),
              !"plot" %in% names(model.frame(m)))
  }
  saveRDS(m, file.path(out, "models", paste0(id, ".rds")))
  vc <- VarCorr(m)$cond
  variance_records[[id]] <- bind_rows(lapply(names(vc), function(g) {
    variables <- strsplit(g, ":", fixed = TRUE)[[1]]
    data.frame(model_id = id, group = g, n_groups = nrow(unique(model_data[variables])),
               variance = vc[[g]][1, 1], sd = sqrt(vc[[g]][1, 1]))
  }))
  ll <- logLik(m)
  fit_records[[id]] <- data.frame(
    model_id = id, dataset = if (is.null(item$species)) "RW+TO" else toupper(item$species),
    n = nobs(m), n_zero = sum(model_data$ba_ha == 0), k = attr(ll, "df"),
    log_likelihood = as.numeric(ll), AIC = AIC(m), convergence = m$fit$convergence,
    pd_hessian = m$sdr$pdHess, max_abs_gradient = max(abs(m$sdr$gradient.fixed)),
    warnings = paste(unique(warnings), collapse = " | "))
  sim <- publication_simulate_residuals(m, n = 1000, seed = seed, refit = FALSE, plot = FALSE)
  tests <- list(uniformity = testUniformity, dispersion = testDispersion,
                outliers = testOutliers, zero_inflation = testZeroInflation)
  diagnostic_records[[id]] <- bind_rows(lapply(names(tests), function(test) {
    result <- tests[[test]](sim, plot = FALSE)
    data.frame(model_id = id, test = test, statistic = unname(result$statistic)[1],
               p_value = result$p.value)
  }))
  png(file.path(out, "diagnostics", paste0(id, "-dharma.png")),
      width = 1800, height = 1000, res = 160)
  plot(sim)
  dev.off()
  png(file.path(out, "diagnostics", paste0(id, "-by-treatment.png")),
      width = 1200, height = 1000, res = 160)
  plotResiduals(sim, form = model_data$treat)
  dev.off()
}
fit_table <- bind_rows(fit_records)
variances <- bind_rows(variance_records)
diagnostics <- bind_rows(diagnostic_records)
stopifnot(all(fit_table$convergence == 0), all(fit_table$pd_hessian))

# A pair of independent species models has a product likelihood on these same
# 128 rows; sum log likelihoods AND parameter counts. Do not compare the 64-row
# individual AICs, or this paired AIC, against the earlier 256-row model AICs.
separate <- filter(fit_table, dataset != "RW+TO")
stopifnot(nrow(separate) == 2L, all(separate$k == 8L))
joint <- filter(fit_table, dataset == "RW+TO")
comparison <- bind_rows(
  transmute(joint, model_id, n, k, log_likelihood, AIC),
  data.frame(model_id = "separate_site_macro_pair", n = sum(separate$n),
    k = sum(separate$k), log_likelihood = sum(separate$log_likelihood), AIC = sum(separate$AIC))
) |>
  mutate(delta_AIC = AIC - min(AIC))
stopifnot(all(comparison$n == 128L),
          max(abs(comparison$AIC - (-2 * comparison$log_likelihood + 2 * comparison$k))) < 1e-8)

# Check the requested fits against a different optimizer and initial SDs.
optimizer_checks <- bind_rows(lapply(c("rw_site_macro", "to_site_macro"), function(id) {
  model_data <- droplevels(d[d$spp == spec[[id]]$species, ])
  m <- models[[id]]
  warnings <- character()
  check <- withCallingHandlers(glmmTMB(spec[[id]]$formula, data = model_data,
    family = ziGamma(link = "log"), ziformula = ~1, dispformula = ~1,
    start = list(theta = rep(log(.3), 2)),
    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"),
                            optCtrl = list(maxit = 10000, reltol = 1e-10))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  difference <- check$fit$objective - m$fit$objective
  saveRDS(check, file.path(out, "models", paste0(id, "-bfgs.rds")))
  stopifnot(check$fit$convergence == 0, abs(difference) < 1e-4)
  data.frame(model_id = id, convergence = check$fit$convergence, pd_hessian = check$sdr$pdHess,
    objective_difference_from_nlminb = difference, warnings = paste(unique(warnings), collapse = " | "))
}))

integrated_means <- function(m, id, species = NULL) {
  frame <- model.frame(m)
  if (!"spp" %in% names(frame)) frame$spp <- species
  grid <- distinct(frame, site, treat, spp, .keep_all = TRUE)
  labels <- distinct(grid, treat, spp)
  A <- t(vapply(seq_len(nrow(labels)), function(i) {
    keep <- grid$treat == labels$treat[i] & grid$spp == labels$spp[i]
    as.numeric(keep) / sum(keep)
  }, numeric(nrow(grid))))
  stopifnot(all(rowSums(A > 0) == 4L), family(m)$link == "log",
            length(VarCorr(m)$zi) == 0L,
            all(vapply(VarCorr(m)$cond, function(v) identical(dim(v), c(1L, 1L)), logical(1))))
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
  estimate <- drop(A %*% mu)
  se <- sqrt(diag(G %*% m$sdr$cov.fixed %*% t(G)))
  functional <- function(b) drop(A %*% (
    exp(drop(X %*% b[ib]) + sum(exp(2 * b[it])) / 2) *
      (1 - plogis(drop(Z %*% b[iz])))))
  numeric_G <- numDeriv::jacobian(functional, unname(b))
  quadrature <- integrate(function(z) exp(sqrt(variance) * z) * dnorm(z), -12, 12)$value
  stopifnot(max(abs(G - numeric_G) / pmax(1, abs(G))) < 1e-5,
            max(abs(mu / exp(variance / 2) - predict(m, grid, type = "response", re.form = NA))) < 1e-7,
            abs(quadrature - exp(variance / 2)) < 1e-8,
            all(is.finite(se)), all(se > 0))
  mutate(labels, model_id = id, estimate = estimate, se = se,
    lcl = estimate * exp(-qnorm(.975) * se / estimate),
    ucl = estimate * exp(qnorm(.975) * se / estimate),
    total_random_variance = variance, integration_multiplier = exp(variance / 2))
}
estimates <- bind_rows(lapply(names(models), function(id) {
  integrated_means(models[[id]], id, spec[[id]]$species)
}))
lead <- read.csv(protected[3]) |>
  filter(spp %in% c("rw", "to")) |>
  transmute(treat, spp, model_id = "joint_four_species_reference", estimate, se, lcl, ucl,
            total_random_variance = random_intercept_variance, integration_multiplier)
display <- bind_rows(lead, estimates) |>
  filter(model_id %in% c("joint_four_species_reference", "joint_full_nested_rwto", "rw_site_macro", "to_site_macro")) |>
  mutate(comparison = ifelse(model_id %in% c("rw_site_macro", "to_site_macro"), "Separate: site + macro plot",
                             ifelse(model_id == "joint_full_nested_rwto", "Joint RW/TO full nesting", "Earlier joint four-species fit")),
         comparison = factor(comparison, levels = c("Earlier joint four-species fit", "Joint RW/TO full nesting", "Separate: site + macro plot")),
         treat = factor(treat, levels = c("gs", "ld", "ha", "hd")),
         spp = factor(spp, levels = c("rw", "to")))
position <- position_dodge(width = .58)
figure <- ggplot(display, aes(treat, estimate, ymin = lcl, ymax = ucl,
                             color = comparison, shape = comparison, group = comparison)) +
  geom_linerange(linewidth = 1, position = position) +
  geom_point(size = 2.4, position = position) +
  facet_wrap(~spp, nrow = 1, scales = "free_y",
             labeller = as_labeller(c(rw = "Redwood", to = "Tanoak"))) +
  scale_color_manual(values = c("#3F51B5", "#C96F38", "#237A57")) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_x_discrete(labels = toupper) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
  labs(x = "Retention treatment", y = expression(Basal~area~(m^2 %.% ha^-1)), color = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold"))
ggsave(file.path(out, "figures/rd02-separate-species-comparison.png"), figure,
       width = 10, height = 4.8, dpi = 300, bg = "white")
ggsave(file.path(out, "figures/rd02-separate-species-comparison.pdf"), figure, width = 10, height = 4.8)
tables <- list("rd02-species-fit-register.csv" = fit_table,
  "rd02-rwto-aic-comparison.csv" = comparison,
  "rd02-species-variance-components.csv" = variances,
  "rd02-species-dharma-tests.csv" = diagnostics,
  "rd02-species-optimizer-checks.csv" = optimizer_checks,
  "rd02-species-integrated-estimates.csv" = estimates)
for (filename in names(tables)) write.csv(tables[[filename]], file.path(out, "tables", filename), row.names = FALSE)
provenance <- list(
  task = "RD02 separate species with site and macro-plot random effects",
  source = "publication/inputs/thesis/regeneration-measurements.rds", source_md5 = unname(protected_hashes[1]),
  script_md5 = unname(tools::md5sum(script_file)),
  data = "64 RW and 64 TO observations; 4 sites, 16 macro plots, 64 vegetation plots per species",
  species_formula = "ba_ha ~ treat + (1 | site/treat); ziGamma(log), zero ~1, dispersion ~1",
  candidate_formulas = lapply(spec, function(item) paste(deparse(item$formula), collapse = " ")),
  joint_comparisons = "all refitted on the SAME 128 RW/TO rows; no AIC comparison with earlier 256-row fits",
  paired_likelihood = "sum species log likelihoods and parameter counts; assumes independence between species",
  diagnostics = "1000 unconditional DHARMa simulations, seed 20261030, plus treatment plots",
  optimizer_sensitivity = "BFGS from all random SDs 0.3; objective agreement checked within 1e-4",
  prediction_checks = "all 32 new model/treatment/species means: predict, numerical gradients, quadrature",
  intervals = "95% log-scale delta method including all conditional random SD uncertainty; approximate near boundary",
  review = "plot-level RE omitted as requested; 4 site and 16 nested macro-plot groups per species",
  selection = "comparison only; original lead figure/estimates and thesis input checksum protected"
)
yaml::write_yaml(provenance, file.path(out, "manifests/rd02-species-comparison-provenance.yml"))
capture.output(sessionInfo(), file = file.path(out, "manifests/rd02-species-comparison-session-info.txt"))
stopifnot(identical(unname(tools::md5sum(protected)), unname(protected_hashes)))
download_dir <- file.path(paths_common$assets, "rd02-species")
for (directory in c("tables", "diagnostics", "figures", "manifests")) {
  target <- file.path(download_dir, directory)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(list.files(file.path(out, directory), full.names = TRUE), target, overwrite = TRUE)))
}
stopifnot(all(file.copy(c(script_file, file.path(script_dir, "README.md")),
                       download_dir, overwrite = TRUE)))
print(comparison)
print(filter(variances, model_id %in% c("rw_site_macro", "to_site_macro")))
print(filter(diagnostics, model_id %in% c("rw_site_macro", "to_site_macro")))
print(filter(estimates, model_id %in% c("rw_site_macro", "to_site_macro")))
