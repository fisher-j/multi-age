# Derive publication-specific fuel components without modifying the thesis
# calculation or modeling pipeline.

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) != 1L) {
  stop("Run with Rscript scripts/fuel/01-derive-components.R")
}

script_dir <- dirname(normalizePath(sub("^--file=", "", script_argument)))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
source(file.path(script_dir, "00-fuel-config.R"))

library(dplyr)
library(readr)
library(tidyr)

paths <- publication_fuel_paths(publication_root)
dir.create(paths$data, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$manifests, recursive = TRUE, showWarnings = FALSE)

# Typed, checksum-validated exports from the separate thesis operation.
transects <- readRDS(paths$input)
legacy_total_load <- readRDS(paths$legacy_load_input)
ids <- publication_fuel_ids

# Keep the field measurements intact for audit. Post-PCT forest-floor values
# are retained here but deliberately receive no duff/litter load conversion.
forest_floor_station <- transects |>
  select(phase, all_of(ids), matches("^(duff_litter|pct_litter|fbd)[12]$")) |>
  pivot_longer(
    cols = matches("^(duff_litter|pct_litter|fbd)[12]$"),
    names_to = c(".value", "station"),
    names_pattern = "(.*?)([12])$"
  ) |>
  transmute(
    phase,
    across(all_of(ids)),
    station = as.integer(station),
    total_forest_floor_depth_cm = duff_litter,
    estimated_litter_fraction = pct_litter / 100,
    estimated_litter_depth_cm = total_forest_floor_depth_cm * estimated_litter_fraction,
    estimated_duff_depth_cm = total_forest_floor_depth_cm - estimated_litter_depth_cm,
    fuel_bed_depth_cm = fbd,
    forest_floor_load_eligible = phase == "prepct"
  )

if (any(forest_floor_station$estimated_litter_fraction < 0 |
  forest_floor_station$estimated_litter_fraction > 1, na.rm = TRUE)) {
  stop("Estimated litter fractions fall outside [0, 1].")
}

# Finney and Martin's cumulative curve partitions the pre-PCT total profile at
# the estimated litter/duff boundary. These are model-derived loads, not direct
# independent bulk-density measurements of the two layers.
forest_floor_pre_station <- forest_floor_station |>
  filter(forest_floor_load_eligible) |>
  mutate(
    total_forest_floor_load_mgha = finney_martin_cumulative_load(
      total_forest_floor_depth_cm,
      total_forest_floor_depth_cm
    ),
    litter_load_mgha = finney_martin_cumulative_load(
      estimated_litter_depth_cm,
      total_forest_floor_depth_cm
    ),
    litter_load_raw_mgha = litter_load_mgha,
    litter_load_bounded_at_zero = litter_load_raw_mgha < 0,
    litter_load_mgha = pmax(litter_load_raw_mgha, 0),
    duff_load_mgha = total_forest_floor_load_mgha - litter_load_mgha
  )

if (any(forest_floor_pre_station$duff_load_mgha < 0, na.rm = TRUE)) {
  stop("Finney-Martin component calculation produced a negative duff load; inspect the input depths before proceeding.")
}

forest_floor_components <- forest_floor_pre_station |>
  select(phase, all_of(ids), station, litter_load_mgha, duff_load_mgha) |>
  pivot_longer(
    cols = ends_with("_load_mgha"),
    names_to = "component",
    values_to = "load_mgha"
  ) |>
  mutate(
    component = recode(
      component,
      litter_load_mgha = "litter",
      duff_load_mgha = "duff"
    ),
    derivation = "Finney-Martin 1993 model 3 cumulative-load partition"
  ) |>
  summarise(
    load_mgha = mean(load_mgha),
    n_stations = sum(!is.na(load_mgha)),
    derivation = first(derivation),
    .by = c(all_of(ids), "phase", "component")
  )

# FireMon-style vegetation calculations remain separate by physiognomic class.
vegetation_station <- transects |>
  select(
    phase,
    all_of(ids),
    matches("^(live_woody|dead_woody|avg_w_ht|live_herb|dead_herb|avg_h_ht)[12]$")
  ) |>
  pivot_longer(
    cols = -c(phase, all_of(ids)),
    names_to = c(".value", "station"),
    names_pattern = "(.*?)([12])$"
  ) |>
  mutate(
    woody_height_m = if_else(live_woody == 0 & dead_woody == 0, 0, avg_w_ht),
    herb_height_m = if_else(live_herb == 0 & dead_herb == 0, 0, avg_h_ht),
    woody_load_mgha = ((live_woody + dead_woody) / 100) * woody_height_m * 18,
    herb_load_mgha = ((live_herb + dead_herb) / 100) * herb_height_m * 8
  ) |>
  select(phase, all_of(ids), station, woody_load_mgha, herb_load_mgha)

vegetation_components <- vegetation_station |>
  pivot_longer(
    cols = ends_with("_load_mgha"),
    names_to = "component",
    values_to = "load_mgha"
  ) |>
  mutate(
    component = recode(
      component,
      woody_load_mgha = "woody_vegetation",
      herb_load_mgha = "herbaceous_vegetation"
    ),
    derivation = "FireMon cover-height bulk-density calculation retained by component"
  ) |>
  summarise(
    load_mgha = mean(load_mgha),
    n_stations = sum(!is.na(load_mgha)),
    derivation = first(derivation),
    .by = c("phase", all_of(ids), "component")
  )

dead_woody_components <- legacy_total_load |>
  select(phase, all_of(ids), onehr, tenhr, hundhr, thoushr) |>
  pivot_longer(
    cols = c(onehr, tenhr, hundhr, thoushr),
    names_to = "component",
    values_to = "load_mgha"
  ) |>
  mutate(
    derivation = "Unchanged thesis-era dead-woody load carried forward from calculate_fuel_load.rda"
  )

# Phase comes from the identifiers retained above. Restrict the exported long
# component table to reportable component/phase combinations.
fuel_components_transect <- bind_rows(
  forest_floor_components,
  vegetation_components,
  dead_woody_components
) |>
  mutate(
    component = factor(
      component,
      levels = c("litter", "duff", "onehr", "tenhr", "hundhr", "thoushr", "woody_vegetation", "herbaceous_vegetation")
    )
  ) |>
  arrange(phase, site, treatment, corner, azi, component)

# Explicit pre/post joins replace the thesis-era lexical phase ordering used to
# construct veg_diff. The primary PCT contrast is woody standing vegetation
# reduction; it is an inferred surface-fuel contribution, not direct slash load.
vegetation_wide <- fuel_components_transect |>
  filter(component %in% c("woody_vegetation", "herbaceous_vegetation")) |>
  select(all_of(ids), phase, component, load_mgha) |>
  pivot_wider(
    names_from = c(component, phase),
    values_from = load_mgha,
    names_sep = "_"
  )

fuel_bed_depth_phase <- forest_floor_station |>
  summarise(
    fuel_bed_depth_cm = mean(fuel_bed_depth_cm),
    n_fuel_bed_stations = sum(!is.na(fuel_bed_depth_cm)),
    .by = c("phase", all_of(ids))
  ) |>
  pivot_wider(
    names_from = phase,
    values_from = c(fuel_bed_depth_cm, n_fuel_bed_stations),
    names_sep = "_"
  )

fuel_paired_transect <- vegetation_wide |>
  filter(
    !is.na(woody_vegetation_prepct),
    !is.na(woody_vegetation_postpct)
  ) |>
  inner_join(fuel_bed_depth_phase, by = ids) |>
  mutate(
    woody_standing_reduction_mgha = woody_vegetation_prepct - woody_vegetation_postpct,
    herbaceous_vegetation_change_mgha = herbaceous_vegetation_postpct - herbaceous_vegetation_prepct,
    interpretation = "Woody standing-vegetation reduction is an inferred potential surface-fuel contribution; it is not a direct slash-load measurement."
  )

pairing_qc <- bind_rows(
  transmute(distinct(filter(vegetation_wide, !is.na(woody_vegetation_prepct)), across(all_of(ids))), status = "pre-PCT woody observation"),
  transmute(distinct(filter(vegetation_wide, !is.na(woody_vegetation_postpct)), across(all_of(ids))), status = "post-PCT woody observation"),
  transmute(anti_join(
    distinct(filter(vegetation_wide, !is.na(woody_vegetation_prepct)), across(all_of(ids))),
    distinct(filter(vegetation_wide, !is.na(woody_vegetation_postpct)), across(all_of(ids))),
    by = ids
  ), status = "pre-PCT woody observation without post-PCT match"),
  transmute(anti_join(
    distinct(filter(vegetation_wide, !is.na(woody_vegetation_postpct)), across(all_of(ids))),
    distinct(filter(vegetation_wide, !is.na(woody_vegetation_prepct)), across(all_of(ids))),
    by = ids
  ), status = "post-PCT woody observation without pre-PCT match"),
  transmute(distinct(fuel_paired_transect, across(all_of(ids))), status = "matched woody pair")
) |>
  count(status, name = "n_transects")

data_dictionary <- tibble(
  component = c("litter", "duff", "woody_vegetation", "herbaceous_vegetation", "woody_standing_reduction_mgha", "fuel_bed_depth_cm"),
  unit = c("Mg ha^-1", "Mg ha^-1", "Mg ha^-1", "Mg ha^-1", "Mg ha^-1", "cm"),
  reporting_phase = c("pre-PCT only", "pre-PCT only", "pre-PCT and post-PCT", "pre-PCT and post-PCT", "matched pre/post", "measurement retained for protocol review"),
  definition = c(
    "Model-derived upper forest-floor load using Finney-Martin cumulative model 3 and the estimated litter-depth boundary. Negative shallow-layer extrapolations are bounded at zero and retained in the station-level audit output.",
    "Model-derived lower forest-floor load: total Finney-Martin forest-floor load minus modeled litter load. Two shallow estimated litter layers were bounded at zero after the equation returned negative mass.",
    "FireMon-style cover-height estimate using the existing woody bulk-density constant.",
    "FireMon-style cover-height estimate using the existing herbaceous bulk-density constant.",
    "Pre-PCT minus post-PCT woody standing vegetation; inferred potential surface-fuel contribution, not direct slash load.",
    "Recorded field value; do not interpret as slash depth until protocol comparability is confirmed."
  )
)

saveRDS(forest_floor_station, file.path(paths$data, "forest-floor-measurements-station.rds"))
saveRDS(forest_floor_pre_station, file.path(paths$data, "forest-floor-pre-components-station.rds"))
saveRDS(fuel_components_transect, file.path(paths$data, "fuel-components-transect.rds"))
saveRDS(fuel_paired_transect, file.path(paths$data, "fuel-paired-transect.rds"))
write_csv(pairing_qc, file.path(paths$data, "fuel-pairing-qc.csv"))
write_csv(data_dictionary, file.path(paths$data, "fuel-component-data-dictionary.csv"))

provenance_lines <- c(
  "input: publication/inputs/thesis/fuel-measurements.rds",
  "legacy-dead-woody-input: publication/inputs/thesis/fuel-deadwood-loads.rds",
  "export_manifest: publication/inputs/thesis/manifest.json",
  "publication_derivation: scripts/fuel/01-derive-components.R",
  "forest_floor_loads: pre-PCT only",
  "forest_floor_method: Finney-Martin 1993 model 3 cumulative-load partition",
  paste0(
    "litter-load-boundary-cases: ",
    sum(forest_floor_pre_station$litter_load_bounded_at_zero)
  ),
  "post-PCT-forest-floor-loads: deliberately omitted pending protocol validity",
  "PCT-primary-contrast: pre-PCT minus post-PCT woody standing vegetation"
)
provenance_connection <- file(file.path(paths$manifests, "input-provenance.yml"), open = "wb")
writeLines(provenance_lines, provenance_connection, sep = "\n", useBytes = TRUE)
close(provenance_connection)
