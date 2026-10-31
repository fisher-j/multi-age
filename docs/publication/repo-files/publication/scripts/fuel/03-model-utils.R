# Publication-local model fitting and diagnostic helpers.

model_output_paths <- function(paths) {
  models <- file.path(paths$output, "models")
  diagnostics <- file.path(paths$output, "diagnostics")
  dir.create(models, recursive = TRUE, showWarnings = FALSE)
  dir.create(diagnostics, recursive = TRUE, showWarnings = FALSE)
  list(models = models, diagnostics = diagnostics)
}

extract_test <- function(test_fun, simulation) {
  result <- tryCatch(test_fun(simulation), error = function(error) error)
  if (inherits(result, "error")) {
    return(list(statistic = NA_real_, p_value = NA_real_, error = conditionMessage(result)))
  }
  list(
    statistic = unname(result$statistic)[1],
    p_value = result$p.value,
    error = NA_character_
  )
}

fit_and_diagnose <- function(
    model_id,
    response,
    analysis,
    data,
    formula,
    family,
    family_label,
    ziformula,
    ziformula_label,
    dispformula,
    dispformula_label,
    paths,
    simulation_n,
    simulation_seed) {
  output_paths <- model_output_paths(paths)
  warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      glmmTMB::glmmTMB(
        formula = formula,
        data = data,
        family = family,
        ziformula = ziformula,
        dispformula = dispformula
      ),
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error) error
  )

  base_record <- tibble::tibble(
    model_id = model_id,
    response = response,
    analysis = analysis,
    n = nrow(data),
    fixed_and_random_formula = paste(deparse(formula), collapse = " "),
    family = family_label,
    ziformula = ziformula_label,
    dispformula = dispformula_label,
    fit_status = if (inherits(fit, "error")) "error" else "fit",
    fit_message = if (inherits(fit, "error")) conditionMessage(fit) else NA_character_,
    warnings = if (length(warnings)) paste(unique(warnings), collapse = " | ") else NA_character_,
    pd_hessian = NA,
    aic = NA_real_,
    dharma_uniformity_p = NA_real_,
    dharma_dispersion_p = NA_real_,
    dharma_outlier_p = NA_real_,
    dharma_zero_inflation_p = NA_real_,
    diagnostic_message = NA_character_
  )

  if (inherits(fit, "error")) {
    return(base_record)
  }

  saveRDS(fit, file.path(output_paths$models, paste0(model_id, ".rds")))
  simulation <- tryCatch(
    publication_simulate_residuals(fit, n = simulation_n, seed = simulation_seed),
    error = function(error) error
  )
  if (inherits(simulation, "error")) {
    return(dplyr::mutate(
      base_record,
      pd_hessian = fit$sdr$pdHess,
      aic = stats::AIC(fit),
      diagnostic_message = conditionMessage(simulation)
    ))
  }

  # Some DHARMa tests may draw internally. Use a null device so these
  # exploratory diagnostics never create an incidental Rplots.pdf in analysis/.
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  uniformity <- extract_test(DHARMa::testUniformity, simulation)
  dispersion <- extract_test(DHARMa::testDispersion, simulation)
  outliers <- extract_test(DHARMa::testOutliers, simulation)
  zero_inflation <- extract_test(DHARMa::testZeroInflation, simulation)

  png(
    filename = file.path(output_paths$diagnostics, paste0(model_id, "-dharma.png")),
    width = 1800,
    height = 900,
    res = 150
  )
  plot(simulation)
  dev.off()

  dplyr::mutate(
    base_record,
    pd_hessian = fit$sdr$pdHess,
    aic = stats::AIC(fit),
    dharma_uniformity_p = uniformity$p_value,
    dharma_dispersion_p = dispersion$p_value,
    dharma_outlier_p = outliers$p_value,
    dharma_zero_inflation_p = zero_inflation$p_value,
    diagnostic_message = paste(
      stats::na.omit(c(uniformity$error, dispersion$error, outliers$error, zero_inflation$error)),
      collapse = " | "
    )
  )
}
