# Publication-only fuel-analysis configuration.
#
# Run the task pages through Quarto. Inputs are committed
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
