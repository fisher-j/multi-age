# here I'm working on functions to bring predicitons and comparisons from the
# marginaleffects package into the emmeans package in order to use the plotting
# functions and p-values from emmeans.

library(emmeans)
library(marginaleffects)


pred <- fuel_tmb$post$mod[[3]] |> predictions(by = "treatment", re.form = NA)
comp <- fuel_tmb$post$mod[[3]] |>
  comparisons(
    # newdata = datagrid(site = NA, corner = NA, treatment = unique),
    variables = list(treatment = "pairwise"),
    by = "contrast",
    re.form = NA
  )

# get an emmobj from marginaleffects
get_emmobj <- function(pred, comp, focal_var) {
  contrast_ord <- correct_contrast_order(pred[[focal_var]], comp$contrast)
  emmeans::emmobj(
    bhat = pred$estimate,
    se.bhat = pred$std.error,
    se.diff = comp$std.error[contrast_ord],
    levels = pred[[focal_var]],
    df = Inf
  )
}

# `combn` provides all pairwise combinations of the input in the order that I
# need, but I need to find the correct order for the constrasts that I have. So
# for each column in `combos`, find the index of `contrast_levels`
# where that level matches both values in the column
correct_contrast_order <- function(pred_levels, contrast_levels) {
  combos <- combn(pred_levels, 2)
  purrr::map_int(
    seq_len(ncol(combos)),
    function(col) {
      which(
        purrr::map_lgl(
          contrast_levels,
          function(contrast_level) {
            all(
              stringr::str_detect(contrast_level, as.character(combos[, col]))
            )
          }
        )
      )
    }
  )
}

pred <- fuel_tmb$pre$mod[[2]] |> predictions(by = "treatment", re.form = NA)
comp <- fuel_tmb$pre$mod[[2]] |>
  comparisons(
    # newdata = datagrid(site = NA, corner = NA, treatment = unique),
    variables = list(treatment = "pairwise"),
    by = "contrast",
    re.form = NA
  )
get_emmobj(pred, comp, "treatment") |> pairs()
get_emmobj(pred, comp, "treatment") |> plot(comparisons = TRUE)
