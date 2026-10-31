# Publication-only fuel-analysis configuration.
#
# Run the numbered scripts through run-publication.bat. Inputs are committed
# thesis exports; no thesis environment or model objects are loaded.

publication_fuel_paths <- function(publication_root) {
  common <- publication_paths(publication_root)
  output <- file.path(common$output, "fuel")
  list(
    input = file.path(common$input, "fuel-measurements.rds"),
    legacy_load_input = file.path(common$input, "fuel-deadwood-loads.rds"),
    output = output,
    data = file.path(output, "data"),
    manifests = file.path(output, "manifests")
  )
}

publication_fuel_ids <- c("site", "treatment", "corner", "azi")

# Finney and Martin (1993), model 3: cumulative forest-floor mass (Mg ha^-1)
# from the surface to depth ds_cm within a profile of total depth dp_cm.
# The model is used only for the pre-PCT forest-floor measurements, where the
# recorded total depth and estimated litter proportion describe the forest floor.
# Its fitted range does not support a negative cumulative mass; the derivation
# script records and bounds such rare shallow-layer extrapolations at zero.
finney_martin_cumulative_load <- function(ds_cm, dp_cm) {
  result <- 6.461 * ds_cm^1.07 - 0.254 * dp_cm
  result[ds_cm == 0] <- 0
  result
}
