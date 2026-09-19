# Publication-only fuel-analysis configuration.
#
# Run the numbered scripts from the analysis/ directory. All publication
# outputs are written beneath analysis/outputs/publication/fuel/; the thesis
# pipeline and its generated objects are read-only inputs.

publication_fuel_paths <- function(analysis_root) {
  list(
    input = file.path(analysis_root, "summarize_collected_data.rda"),
    output = file.path(analysis_root, "outputs", "publication", "fuel"),
    data = file.path(analysis_root, "outputs", "publication", "fuel", "data"),
    manifests = file.path(
      analysis_root,
      "outputs",
      "publication",
      "fuel",
      "manifests"
    )
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
