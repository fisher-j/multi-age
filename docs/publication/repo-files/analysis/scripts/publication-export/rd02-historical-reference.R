# Historical RD02 reference calculation, moved without changing its mathematics.
# Called only by the thesis-environment exporter; never by publication rendering.
export_rd02_reference <- function(saved, output) {
  model <- saved$mba
  out <- file.path(output, "regeneration")
  for (directory in c("tables", "inference-audit", "manifests"))
    dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
# Audit this model independently of the fuel helpers: its predictors are
# treat/spp and its sole grouping factor is site:spp, not the fuel hierarchy.
vc <- VarCorr(model)
par <- model$fit$par
ib <- which(names(par) == "beta")
iz <- which(names(par) == "betazi")
it <- which(names(par) == "theta")
stopifnot(
  family(model)$family == "Gamma", family(model)$link == "log",
  identical(names(vc$cond), "site:spp"),
  identical(dim(vc$cond[[1]]), c(1L, 1L)),
  identical(colnames(vc$cond[[1]]), "(Intercept)"),
  length(vc$zi) == 0L, length(vc$disp) == 0L, length(it) == 1L,
  model$fit$convergence == 0, model$sdr$pdHess,
  isTRUE(all.equal(unname(par), unname(model$sdr$par.fixed))),
  is.null(model.offset(model$frame))
)
V <- model$sdr$cov.fixed
stopifnot(all(is.finite(V)), min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) > 0)

# One valid observed row per site/treatment/species, equal weights for sites.
# There are no site fixed effects here, so all four site expectations coincide
# after integrating the site-by-species intercept distribution.
grid <- distinct(model$frame, site, treat, spp, .keep_all = TRUE)
labels <- distinct(grid, treat, spp)
A <- t(vapply(seq_len(nrow(labels)), function(i) {
  keep <- grid$treat == labels$treat[i] & grid$spp == labels$spp[i]
  as.numeric(keep) / sum(keep)
}, numeric(nrow(grid))))
stopifnot(nrow(grid) == 64L, nrow(labels) == 16L,
          all(rowSums(A > 0) == 4L), all(rowSums(A) == 1))
X <- model.matrix(delete.response(terms(formula(model, fixed.only = TRUE))), grid)
Z <- model.matrix(formula(model, component = "zi", fixed.only = TRUE), grid)
stopifnot(identical(colnames(X), names(fixef(model)$cond)),
          identical(colnames(Z), names(fixef(model)$zi)))
variance <- sum(exp(2 * par[it]))
stopifnot(abs(variance - vc$cond[[1]][1, 1]) < 1e-10)
multiplier <- exp(variance / 2)
eta <- drop(X %*% par[ib])
pzero <- plogis(drop(Z %*% par[iz]))
mu0 <- exp(eta) * (1 - pzero)
mu <- mu0 * multiplier

# Analytic gradient in the FULL fitted parameter space: includes the zero
# coefficients, log random-effect SD, and their covariance with beta.
D <- matrix(0, nrow(grid), length(par))
D[, ib] <- mu * X
D[, iz] <- -mu * pzero * Z
D[, it] <- mu * variance
G <- A %*% D
est <- drop(A %*% mu)
se <- sqrt(diag(G %*% V %*% t(G)))
estimates <- mutate(labels,
  estimate = est, se = se,
  lcl = est * exp(-qnorm(.975) * se / est),
  ucl = est * exp(qnorm(.975) * se / est),
  unit = "m2 ha-1", n_sites = 4L,
  model_id = "thesis_mba_m5", random_intercept_variance = variance,
  integration_multiplier = multiplier,
  estimand = "random-effect-integrated expected basal area including zeros",
  interval_method = "log-scale delta method using full parameter covariance"
)
stopifnot(all(is.finite(se)), all(se > 0),
          all(estimates$lcl > 0), all(estimates$lcl < est), all(estimates$ucl > est))

# Reproduce the thesis zero-random-effect target using predict() and
# marginaleffects, then verify integration and its uncertainty independently.
pred <- predict(model, grid, type = "response", re.form = NA,
                se.fit = TRUE, cov.fit = TRUE)
reference <- drop(A %*% pred$fit)
reference_se <- sqrt(diag(A %*% pred$cov.fit %*% t(A)))
me_ref <- as.data.frame(avg_predictions(model, newdata = grid,
  by = c("treat", "spp"), type = "response", re.form = NA))
key <- function(x) paste(x$treat, x$spp, sep = ":")
me_ref <- me_ref[match(key(labels), key(me_ref)), ]
functional <- function(x) {
  b <- x$estimate
  row_mean <- exp(drop(X %*% b[ib]) + sum(exp(2 * b[it])) / 2) *
    (1 - plogis(drop(Z %*% b[iz])))
  data.frame(term = seq_len(nrow(labels)), estimate = drop(A %*% row_mean))
}
me_int <- as.data.frame(hypotheses(
  data.frame(term = paste0("p", seq_along(par)), estimate = unname(par)),
  hypothesis = functional, vcov = V, numderiv = "fdcenter"))
numerical_gradient <- numDeriv::jacobian(function(b) {
  functional(data.frame(estimate = b))$estimate
}, unname(par))
quadrature <- integrate(function(z) exp(sqrt(variance) * z) * dnorm(z),
                        -12, 12, rel.tol = 1e-10)$value
stopifnot(
  max(abs(mu0 - pred$fit)) < 1e-8,
  max(abs(reference - me_ref$estimate)) < 1e-8,
  max(abs(reference_se - me_ref$std.error) / pmax(1, reference_se)) < 1e-4,
  max(abs(est - me_int$estimate)) < 1e-8,
  max(abs(se - me_int$std.error) / pmax(1, se)) < 1e-4,
  max(abs(G - numerical_gradient) / pmax(1, abs(G))) < 1e-5,
  abs(quadrature - multiplier) < 1e-9,
  max(abs(est / reference - multiplier)) < 1e-8
)
audit <- mutate(labels, zero_effect_reference = reference,
  zero_effect_se = reference_se, reference_marginaleffects = me_ref$estimate,
  integrated_mean = est, integrated_se = se,
  integrated_marginaleffects = me_int$estimate,
  integrated_marginaleffects_se = me_int$std.error,
  integration_multiplier = multiplier,
  integrated_lcl = estimates$lcl, integrated_ucl = estimates$ucl)

# Keep all four species in the record, even though only rw/to enter the lead
# figure. Basal area is area density, not a stem-count model or trees/ha.
species_order <- c("rw", "to", "df", "other")
estimates <- estimates |>
  mutate(spp = factor(spp, levels = species_order)) |>
  arrange(spp, treat)
write.csv(estimates, file.path(out, "tables/rd02-basal-area-model-estimates.csv"), row.names = FALSE)
write.csv(audit, file.path(out, "inference-audit/rd02-estimand-comparison.csv"), row.names = FALSE)
write.csv(grid[, c("site", "treat", "spp")],
          file.path(out, "inference-audit/rd02-reference-grid.csv"), row.names = FALSE)
observed <- saved$regen_pa |>
  group_by(spp, treat) |>
  summarise(n_vegetation_plots = n(), n_sites = n_distinct(site),
            n_zero = sum(ba_ha == 0), observed_mean = mean(ba_ha), .groups = "drop")
stopifnot(all(observed$n_vegetation_plots == 16L), all(observed$n_sites == 4L))
write.csv(observed, file.path(out, "tables/rd02-observed-basal-area-summary.csv"), row.names = FALSE)


  saveRDS(list(logLik = as.numeric(logLik(model)),
               response = unname(model.response(model.frame(model))),
               formula = deparse1(formula(model)),
               zero = deparse1(formula(model, component = "zi")),
               dispersion = deparse1(formula(model, component = "disp"))),
          file.path(out, "saved-model-reference.rds"))
}
