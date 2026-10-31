# Thesis fuel Results replay; executed only in the thesis environment.
export_fuel_reference <- function(analysis_root, output) {
  audit_dir <- file.path(output, "fuel")
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
thesis <- new.env()
load(file.path(analysis_root, "fuel_modeling_round_3.rda"), envir = thesis)
thesis_results <- list()
for (phase in c("pre", "post")) {
  mods <- thesis$fuel_tmb[[phase]]
  for (i in seq_len(nrow(mods))) {
    m <- mods$mod[[i]]
    grid <- expand(mods$data[[i]], treatment, site)
    # Both historical approaches use observed, valid treatment/site pairs;
    # nested random-only identifiers are not crossed in the thesis fuel grid.
    stopifnot(nrow(anti_join(grid, distinct(m$frame, treatment, site),
                            by = c("treatment", "site"))) == 0L)
    p <- as.data.frame(avg_predictions(m, newdata = grid, by = "treatment",
                                      re.form = NA, type = "response"))
    thesis_results[[paste(phase, i)]] <- tibble(
      phase = paste0(phase, "pct"), thesis_class = as.character(mods$class[i]),
      treatment = as.character(p$treatment), thesis_reference = p$estimate,
      thesis_se = p$std.error, formula = deparse1(formula(m)),
      zero = deparse1(formula(m, component = "zi"))
    )
  }
}
thesis_results <- bind_rows(thesis_results)
write_csv(thesis_results, file.path(audit_dir, "thesis-reference-reproduction.csv"))

}
