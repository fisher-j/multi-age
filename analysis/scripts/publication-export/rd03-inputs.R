# Export only measurements and plain historical evidence in thesis R 4.3.2.
# Publication audit calculations live in task-rd03.qmd; no model is refitted here.
export_rd03_inputs <- function(output) {
  out <- file.path(output, "sprouts")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  workbook <- "../data/Sprouts10yr.xlsx"
  sheets <- readxl::excel_sheets(workbook)
  stopifnot(identical(sheets, c("Age10all", "Age10tallest50tpha")))
  raw <- as.data.frame(readxl::read_excel(workbook, sheet = "Age10all"))
  tallest <- as.data.frame(readxl::read_excel(workbook, sheet = "Age10tallest50tpha"))
  saved <- new.env(); load("wrangled_sprouts.rda", envir = saved)
  models <- new.env(); load("sprout_modeling.rda", envir = models)
  d <- as.data.frame(saved$sprouts)
  # Reconcile every imported column, including the historical renumbered IDs.
  map <- c(site = "Site", plot = "Plot", treat = "Trtmt", tree = "Tree",
    spp = "Species", ht1 = "HT1yr_m", ht5 = "HT5yr_m", ht10 = "HT10yr_m",
    ht_inc5 = "HTI1-5yr", ht_inc10 = "HTI5-10yr", dbh10 = "DBH10yr_cm",
    lcbh10 = "LCBH10yr", sdi_init = "SDIinit", agg = "AggregatedYN", ht_rnk10 = "HT10rank")
  replay <- raw[, unname(map)]; names(replay) <- names(map)
  replay <- replay |> mutate(plot = cur_group_id(), .by = c(site, treat)) |>
    mutate(tree = row_number())
  for (name in names(map)) {
    if (is.factor(d[[name]])) replay[[name]] <- factor(replay[[name]], levels = levels(d[[name]]))
    stopifnot(identical(replay[[name]], d[[name]]))
  }
  key <- function(x) paste(x$Site, x$Plot, x$Species, x$Tree, sep = ":")
  stopifnot(nrow(raw) == 692L, !anyDuplicated(key(raw)), !anyDuplicated(key(tallest)))
  index <- match(key(tallest), key(raw))
  stopifnot(!anyNA(index), identical(names(raw), names(tallest)))
  # Preserve the selected-height sheet separately: spreadsheet formula precision
  # can differ in last bits. It is not a second field cohort.
  stopifnot(max(abs(as.matrix(tallest[, c("HT1yr_m", "HT5yr_m", "HT10yr_m")]) -
                       as.matrix(raw[index, c("HT1yr_m", "HT5yr_m", "HT10yr_m")]))) < 1e-12)
  d$source_row <- seq_len(nrow(raw)) + 1L
  d$original_plot <- raw$Plot
  d$original_tree <- raw$Tree
  d$residual_rw <- raw$ResidRWonClumpYN
  d$in_tallest_sheet <- key(raw) %in% key(tallest)
  frames <- lapply(c("dht", "dinc", "dht10"), function(name) as.data.frame(models[[name]]))
  names(frames) <- c("dht", "dinc", "dht10")
  stopifnot(identical(as.data.frame(saved$lengthen_data(saved$sprouts, "ht")), frames$dht),
    identical(as.data.frame(saved$lengthen_data(saved$sprouts, "ht_inc") |>
      mutate(year = as.factor(year))), frames$dinc),
    isTRUE(all.equal(frames$dht[frames$dht$year == 10, ], frames$dht10, check.attributes = FALSE)))
  references <- list(); records <- list()
  for (name in c("minc_sel", "ht10_sel")) {
    model <- models[[name]]
    data_frame <- if (name == "minc_sel") frames$dinc else frames$dht10
    for (column in names(model$frame))
      stopifnot(identical(model$frame[[column]], data_frame[[column]]))
    by <- if (name == "minc_sel") c("treat", "spp", "year") else c("treat", "spp")
    grid <- distinct(model$frame, across(all_of(by)), .keep_all = TRUE)
    pred <- predict(model, newdata = grid, re.form = NA, type = "response", se.fit = TRUE)
    X <- model.matrix(delete.response(terms(formula(model, fixed.only = TRUE))), grid)
    direct <- family(model)$linkinv(drop(X %*% fixef(model)$cond))
    stopifnot(max(abs(direct - pred$fit)) < 1e-8,
      model$fit$convergence == 0, model$sdr$pdHess,
      all(is.finite(pred$fit)), all(is.finite(pred$se.fit)))
    references[[name]] <- data.frame(model_id = name, treat = grid$treat, spp = grid$spp,
      period = if (name == "minc_sel") as.character(grid$year) else "year10",
      estimate = as.numeric(pred$fit), se = as.numeric(pred$se.fit),
      unit = if (name == "minc_sel") "m yr-1" else "m",
      estimand = "historical conditional mean with random effects set to zero")
    records[[name]] <- data.frame(model_id = name, mean = deparse1(formula(model)),
      dispersion = deparse1(formula(model, component = "disp")),
      family = family(model)$family, link = family(model)$link,
      n = nobs(model), log_likelihood = as.numeric(logLik(model)),
      convergence = model$fit$convergence, positive_definite_hessian = model$sdr$pdHess)
  }
  saveRDS(raw, file.path(out, "workbook-all.rds"), version = 3)
  saveRDS(tallest, file.path(out, "workbook-tallest.rds"), version = 3)
  saveRDS(d, file.path(out, "measurements.rds"), version = 3)
  saveRDS(frames, file.path(out, "historical-frames.rds"), version = 3)
  write.csv(bind_rows(references), file.path(out, "historical-predictions.csv"), row.names = FALSE)
  write.csv(bind_rows(records), file.path(out, "historical-model-record.csv"), row.names = FALSE)
  list(sprout_measurements = d, sprout_workbook_all = raw, sprout_workbook_tallest = tallest,
       sprout_historical_heights = frames$dht, sprout_historical_increments = frames$dinc,
       sprout_historical_year10 = frames$dht10)
}
