# Keep valid nested identifiers from one observed row per site/treatment/phase.
# These identifiers do not contribute fitted random effects when re.form=NA.
reference_grid <- function(model, phase = NULL) {
  keys <- intersect(c("site", "treatment", "phase"), names(model$frame))
  grid <- distinct(model$frame, across(all_of(keys)), .keep_all = TRUE)
  if (!is.null(phase) && "phase" %in% keys) grid <- filter(grid, .data$phase == .env$phase)
  counts <- count(grid, treatment)
  stopifnot(length(unique(counts$n)) == 1L)
  grid
}

# A maps row predictions to equally weighted site means. A V A' retains every
# cross-site covariance; treating site predictions as independent is incorrect.
averaging_matrix <- function(grid) {
  keys <- intersect(c("treatment", "phase"), names(grid))
  labels <- distinct(grid, across(all_of(keys)))
  A <- t(vapply(seq_len(nrow(labels)), function(i) {
    keep <- Reduce(`&`, lapply(keys, function(k) grid[[k]] == labels[[k]][i]))
    as.numeric(keep) / sum(keep)
  }, numeric(nrow(grid))))
  list(A = A, labels = labels)
}

custom_reference <- function(model, grid) {
  p <- predict(model, newdata = grid, type = "response", re.form = NA,
               allow.new.levels = TRUE, se.fit = TRUE, cov.fit = TRUE)
  avg <- averaging_matrix(grid)
  transform(avg$labels, estimate = as.vector(avg$A %*% p$fit),
            std.error = sqrt(diag(avg$A %*% p$cov.fit %*% t(avg$A))))
}

# Exact integration for THESE models: independent Gaussian random intercepts in
# the conditional model, log/identity link, no random effects in the zero model.
# This deliberately fails on slopes, correlated effects, offsets, or other links.
integration_setup <- function(model, grid) {
  vc <- VarCorr(model)
  stopifnot(length(vc$zi) == 0L, length(vc$disp) == 0L,
            all(vapply(vc$cond, function(x) identical(dim(x), c(1L, 1L)), logical(1))))
  X <- model.matrix(delete.response(terms(formula(model, fixed.only = TRUE))), grid)
  Z <- model.matrix(formula(model, component = "zi", fixed.only = TRUE), grid)
  par <- model$fit$par
  ib <- which(names(par) == "beta")
  iz <- which(names(par) == "betazi")
  it <- which(names(par) == "theta")
  stopifnot(ncol(X) == length(ib), ncol(Z) == length(iz),
            length(it) == length(vc$cond), model$sdr$pdHess,
            isTRUE(all.equal(unname(par), unname(model$sdr$par.fixed))),
            is.null(model.offset(model$frame)),
            family(model)$link %in% c("log", "identity"))
  variance <- sum(vapply(vc$cond, function(x) x[1, 1], numeric(1)))
  stopifnot(isTRUE(all.equal(variance, sum(exp(2 * par[it])), tolerance = 1e-8)))
  list(X = X, Z = Z, par = par, V = model$sdr$cov.fixed,
       ib = ib, iz = iz, it = it, link = family(model)$link,
       avg = averaging_matrix(grid), variance = variance)
}

# Independent analytic derivatives for the custom delta-method implementation.
custom_integrated <- function(s) {
  eta <- drop(s$X %*% s$par[s$ib])
  pzero <- if (length(s$iz)) plogis(drop(s$Z %*% s$par[s$iz])) else rep(0, length(eta))
  if (s$link == "log") {
    mu <- exp(eta + s$variance / 2) * (1 - pzero)
    D <- matrix(0, nrow(s$X), length(s$par))
    D[, s$ib] <- mu * s$X
    if (length(s$iz)) D[, s$iz] <- -mu * pzero * s$Z
    D[, s$it] <- outer(mu, exp(2 * s$par[s$it]))
  } else {
    stopifnot(length(s$iz) == 0L)
    mu <- eta
    D <- matrix(0, nrow(s$X), length(s$par))
    D[, s$ib] <- s$X
  }
  G <- s$avg$A %*% D
  list(estimate = drop(s$avg$A %*% mu),
       se = sqrt(pmax(0, diag(G %*% s$V %*% t(G)))), G = G)
}

# Contrast the same matched model's integrated phase means, retaining their
# covariance through a single gradient and the full parameter covariance.
woody_change_setup <- function(s) {
  labels <- s$avg$labels
  stopifnot(all(c("phase", "treatment") %in% names(labels)),
            setequal(unique(as.character(labels$phase)), c("prepct", "postpct")))
  treatments <- unique(as.character(labels$treatment))
  C <- t(vapply(treatments, function(trt) {
    stopifnot(sum(labels$treatment == trt & labels$phase == "prepct") == 1L,
              sum(labels$treatment == trt & labels$phase == "postpct") == 1L)
    as.numeric(labels$treatment == trt) * ifelse(labels$phase == "prepct", 1, -1)
  }, numeric(nrow(labels))))
  s$avg$A <- C %*% s$avg$A
  s$avg$labels <- data.frame(treatment = treatments)
  s
}

# marginaleffects does the nonlinear delta method on a supplied full parameter
# table, including theta (log SDs), not just the conditional fixed coefficients.
# avg_predictions(re.form=NA) alone does NOT integrate over random effects.
marginaleffects_integrated <- function(s) {
  parameters <- data.frame(term = paste0("p", seq_along(s$par)),
                           estimate = unname(s$par))
  functional <- function(x) {
    b <- x$estimate
    eta <- drop(s$X %*% b[s$ib])
    pzero <- if (length(s$iz)) plogis(drop(s$Z %*% b[s$iz])) else rep(0, length(eta))
    mu <- if (s$link == "log") exp(eta + sum(exp(2 * b[s$it])) / 2) * (1 - pzero) else eta
    data.frame(term = seq_len(nrow(s$avg$labels)), estimate = drop(s$avg$A %*% mu))
  }
  marginaleffects::hypotheses(parameters, hypothesis = functional, vcov = s$V,
             numderiv = "fdcenter", conf_level = .95)
}

