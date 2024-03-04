# function to access mostly raw data, but with combined veg and thoushr fuels.
# and select the output variables (`...`)
load2 <- function(shape = "wide", ...) {
  load_vars <- c("onehr", "tenhr", "hundhr", "dufflitter", "thoushr", "veg")
  tl <- pivot_wider(total_load, names_from = class, values_from = load) |>
    mutate(
      thoushr = rowSums(pick(c(thoushr_s, thoushr_r)), na.rm = TRUE),
      veg = rowSums(pick(c(woody, herb)), na.rm = TRUE),
      .keep = "unused"
    ) |>
    mutate(treatment = forcats::fct_relevel(treatment, c("gs", "ld", "ha", "hd")))
  if (!missing(...)) tl <- select(tl, ...)
  if (shape == "long") {
    tl <- pivot_longer(tl,
      -any_of(c("site", "treatment", "corner", "azi")),
      names_to = "class",
      values_to = "load"
    )
    load_vars <- load_vars[load_vars %in% tl$class]
    tl <- mutate(tl, class = factor(class, levels = load_vars))
  }
  tl
}
