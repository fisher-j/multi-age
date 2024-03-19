# function to access mostly raw data, but with combined veg and thoushr fuels.
# and select the output variables (`...`)
load2 <- function(shape = "wide", pct = c("pre", "post", "both"), ...) {
  pct <- pct[1]
  pct_filter <- switch(pct,
    pre = "prepct", post = "postpct", both = "prepct|postpct"
  )
  tl <- filter(total_load, phase == pct_filter)
  load_vars <- c("onehr", "tenhr", "hundhr", "dufflitter", "thoushr", "veg", "veg_diff")
  # TODO: if(pct == "post")
  if (!missing(...)) tl <- select(tl, ...)
  if (shape == "long") {
    tl <- pivot_longer(tl,
      -any_of(c("phase", "site", "treatment", "corner", "azi")),
      names_to = "class",
      values_to = "load"
    )
    load_vars <- load_vars[load_vars %in% tl$class]
    tl <- mutate(tl, class = factor(class, levels = load_vars)) |>
      filter(!is.na(load))
  }
  tl
}
