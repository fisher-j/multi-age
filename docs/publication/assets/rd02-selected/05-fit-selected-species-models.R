# Selected RD02 analysis: four independent species-category models.
# From analysis/: Rscript scripts/publication/regeneration/05-fit-selected-species-models.R
# Also sourceable. Preserves thesis data and the historical joint-model record.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else sys.frame(1)$ofile
script_dir <- dirname(normalizePath(script_file))
analysis_root <- normalizePath(file.path(script_dir, "..", "..", ".."))
suppressPackageStartupMessages({library(glmmTMB); library(DHARMa); library(dplyr); library(ggplot2)})
input <- file.path(analysis_root, "regen_visualize.rda")
protected <- c(input, file.path(analysis_root, c(
  "outputs/publication/regeneration/tables/rd02-basal-area-model-estimates.csv",
  "outputs/publication/regeneration/figures/rd02-regeneration-basal-area.png")))
protected_hashes <- tools::md5sum(protected)
saved <- new.env()
load(input, envir = saved)
d <- as.data.frame(saved$regen_pa)
d$site <- factor(d$site)
d$plot <- factor(d$plot)
species_order <- c("rw", "to", "df", "other")
species_labels <- c(rw = "Redwood", to = "Tanoak", df = "Douglas-fir", other = "Other species")
stopifnot(nrow(d) == 256L, !anyNA(d), setequal(unique(d$spp), species_order))
out <- file.path(analysis_root, "outputs/publication/regeneration/selected-species-models")
for (directory in c("models", "tables", "diagnostics", "figures", "manifests")) {
  dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
}
models <- list()
fit_records <- variance_records <- diagnostic_records <- optimizer_records <- list()
seed <- 20261030L
for (sp in species_order) {
  message("Fitting selected species category: ", sp)
  data_sp <- droplevels(d[d$spp == sp, ])
  stopifnot(nrow(data_sp) == 64L, n_distinct(data_sp$site) == 4L,
            nrow(distinct(data_sp, site, treat)) == 16L,
            all(count(data_sp, site, treat, plot)$n == 1L))
  warnings <- character()
  m <- withCallingHandlers(glmmTMB(
    ba_ha ~ treat + (1 | site/treat), data = data_sp,
    family = ziGamma(link = "log"), ziformula = ~1, dispformula = ~1,
    control = glmmTMBControl(optCtrl = list(iter.max = 10000, eval.max = 10000))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  stopifnot(m$fit$convergence == 0, m$sdr$pdHess,
            setequal(names(VarCorr(m)$cond), c("site", "treat:site")),
            !"plot" %in% names(model.frame(m)))
  models[[sp]] <- m
  saveRDS(m, file.path(out, "models", paste0(sp, "_site_macro.rds")))
  ll <- logLik(m)
  fit_records[[sp]] <- data.frame(spp = sp, species = species_labels[sp], model_id = paste0(sp, "_site_macro"),
    n = nobs(m), n_zero = sum(data_sp$ba_ha == 0), k = attr(ll, "df"),
    log_likelihood = as.numeric(ll), AIC = AIC(m), convergence = m$fit$convergence,
    pd_hessian = m$sdr$pdHess, max_abs_gradient = max(abs(m$sdr$gradient.fixed)),
    warnings = paste(unique(warnings), collapse = " | "), row.names = NULL)
  variance_records[[sp]] <- bind_rows(lapply(names(VarCorr(m)$cond), function(g) {
    value <- VarCorr(m)$cond[[g]][1, 1]
    data.frame(spp = sp, group = g, n_groups = nrow(unique(data_sp[strsplit(g, ":", fixed = TRUE)[[1]]])),
               variance = value, sd = sqrt(value))
  }))
  sim <- simulateResiduals(m, n = 1000, seed = seed, refit = FALSE, plot = FALSE)
  tests <- list(uniformity = testUniformity, dispersion = testDispersion,
                outliers = testOutliers, zero_inflation = testZeroInflation)
  diagnostic_records[[sp]] <- bind_rows(lapply(names(tests), function(test) {
    result <- tests[[test]](sim, plot = FALSE)
    data.frame(spp = sp, test = test, statistic = unname(result$statistic)[1], p_value = result$p.value)
  }))
  png(file.path(out, "diagnostics", paste0(sp, "-dharma.png")), width = 1800, height = 1000, res = 160)
  plot(sim)
  dev.off()
  png(file.path(out, "diagnostics", paste0(sp, "-by-treatment.png")), width = 1200, height = 1000, res = 160)
  plotResiduals(sim, form = data_sp$treat)
  dev.off()
  check_warnings <- character()
  alternative <- withCallingHandlers(glmmTMB(
    ba_ha ~ treat + (1 | site/treat), data = data_sp,
    family = ziGamma(link = "log"), ziformula = ~1, dispformula = ~1,
    start = list(theta = rep(log(.3), 2)),
    control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"),
                            optCtrl = list(maxit = 10000, reltol = 1e-10))),
    warning = function(w) { check_warnings <<- c(check_warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  objective_difference <- alternative$fit$objective - m$fit$objective
  stopifnot(alternative$fit$convergence == 0, abs(objective_difference) < 1e-4)
  optimizer_records[[sp]] <- data.frame(spp = sp, convergence = alternative$fit$convergence,
    pd_hessian = alternative$sdr$pdHess, objective_difference_from_nlminb = objective_difference,
    warnings = paste(unique(check_warnings), collapse = " | "))
}

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

estimates <- bind_rows(lapply(species_order, function(sp) {
  integrated_means(models[[sp]], paste0(sp, "_site_macro"), sp)
})) |>
  mutate(spp = factor(spp, levels = species_order),
         treat = factor(treat, levels = c("gs", "ld", "ha", "hd")),
         species = unname(species_labels[as.character(spp)]), unit = "m2 ha-1",
         estimand = "species-specific arithmetic expected basal area including zeros, integrated over site and macro-plot random effects",
         interval_method = "95% log-scale delta method using full parameter covariance") |>
  arrange(spp, treat)
stopifnot(nrow(estimates) == 16L, !anyDuplicated(estimates[c("spp", "treat")]),
          all(estimates$lcl > 0), all(estimates$lcl <= estimates$estimate), all(estimates$ucl >= estimates$estimate))
# The selected RW/TO fits must reproduce the accepted sensitivity specification.
reference <- read.csv(file.path(analysis_root,
  "outputs/publication/regeneration/separate-species-comparison/tables/rd02-species-integrated-estimates.csv"))
key <- function(x) paste(x$spp, x$treat)
for (sp in c("rw", "to")) {
  old <- reference[reference$model_id == paste0(sp, "_site_macro"), ]
  now <- estimates[estimates$spp == sp, ]
  old <- old[match(key(now), key(old)), ]
  stopifnot(max(abs(now$estimate - old$estimate)) < 1e-6,
            max(abs(now$se - old$se)) < 1e-6)
}
fit_table <- bind_rows(fit_records)
variances <- bind_rows(variance_records)
diagnostics <- bind_rows(diagnostic_records)
optimizer_checks <- bind_rows(optimizer_records)
stopifnot(all(fit_table$k == 8L), nrow(variances) == 8L, nrow(diagnostics) == 16L)
write.csv(estimates, file.path(out, "tables/rd02-selected-species-estimates.csv"), row.names = FALSE)
write.csv(fit_table, file.path(out, "tables/rd02-selected-species-fit-register.csv"), row.names = FALSE)
write.csv(variances, file.path(out, "tables/rd02-selected-species-variances.csv"), row.names = FALSE)
write.csv(diagnostics, file.path(out, "tables/rd02-selected-species-diagnostics.csv"), row.names = FALSE)
write.csv(optimizer_checks, file.path(out, "tables/rd02-selected-species-optimizer-checks.csv"), row.names = FALSE)
figure <- ggplot(filter(estimates, spp %in% c("rw", "to")),
  aes(treat, estimate, ymin = lcl, ymax = ucl, color = spp, group = spp)) +
  geom_linerange(linewidth = 1.6, position = position_dodge(width = .44)) +
  geom_point(aes(shape = spp), fill = "black", color = "white", stroke = .6, size = 2.8,
             position = position_dodge(width = .44)) +
  facet_wrap(~spp, nrow = 1, scales = "free_y",
             labeller = as_labeller(c(rw = "Redwood", to = "Tanoak"))) +
  scale_color_manual(values = c(rw = "#3F51B5", to = "#C96F38"), labels = c(rw = "Redwood", to = "Tanoak")) +
  scale_shape_manual(values = c(rw = 21, to = 24), labels = c(rw = "Redwood", to = "Tanoak")) +
  scale_x_discrete(labels = toupper) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .06))) +
  labs(x = "Retention treatment", y = expression(Basal~area~(m^2 %.% ha^-1)), color = NULL, shape = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold"))
figure_name <- "rd02-selected-species-basal-area"
ggsave(file.path(out, "figures", paste0(figure_name, ".png")), figure, width = 7, height = 4.4, dpi = 320, bg = "white")
ggsave(file.path(out, "figures", paste0(figure_name, ".pdf")), figure, width = 7, height = 4.4)
provenance <- list(
  task = "RD02 selected species-category models", selected_structure = "four independent species-category models",
  selection_basis = "User selected biological interpretability and conventional species-specific analysis; not minimum AIC",
  source = "analysis/regen_visualize.rda", source_md5 = unname(protected_hashes[1]),
  script_md5 = unname(tools::md5sum(script_file)),
  categories = unname(species_labels), formula = "ba_ha ~ treat + (1 | site/treat)",
  family = "zero-inflated Gamma; conditional log link; zero ~1; dispersion ~1",
  design = "64 vegetation-plot observations per category; 4 site and 16 nested macro-plot intercept groups; no plot-level RE",
  target = "arithmetic expected basal area including zeros and analytically integrating site and macro-plot intercepts",
  intervals = "95% log-scale delta method; full parameter covariance including both random SDs",
  checks = "16 mean cells: predict, numerical gradients, quadrature; BFGS optimizer checks; RW/TO sensitivity agreement",
  diagnostics = "1000 unconditional DHARMa simulations per category; seed 20261030; residual plots by treatment",
  artifacts = "RW/TO figure in Results; four-category estimate table in supplement",
  figure_layout = "two species panels with different vertical scales to retain interval legibility",
  status = "model structure selected; updated displays and diagnostics available for review"
)
yaml::write_yaml(provenance, file.path(out, "manifests/rd02-selected-species-provenance.yml"))
capture.output(sessionInfo(), file = file.path(out, "manifests/rd02-selected-species-session-info.txt"))
stopifnot(identical(unname(tools::md5sum(protected)), unname(protected_hashes)))
asset_root <- file.path(analysis_root, "../publication/assets")
stopifnot(file.copy(file.path(out, "figures", paste0(figure_name, ".png")), asset_root, overwrite = TRUE))
download_dir <- file.path(asset_root, "rd02-selected")
for (directory in c("tables", "diagnostics", "figures", "manifests")) {
  target <- file.path(download_dir, directory)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(list.files(file.path(out, directory), full.names = TRUE), target, overwrite = TRUE)))
}
stopifnot(file.copy(script_file, download_dir, overwrite = TRUE))
print(fit_table)
print(diagnostics)
print(as.data.frame(select(estimates, species, treat, estimate, lcl, ucl)))
