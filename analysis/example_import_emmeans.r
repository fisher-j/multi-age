library(emmeans)
library(dplyr)
library(tidyr)
library(patchwork)

MOats.lm <- lm(yield ~ Block + Variety, data = MOats)
MOats.rg <- ref_grid(MOats.lm, mult.name = "nitro")

pred <- emmeans(MOats.rg, ~ nitro | Variety) |>
  data.frame() |>
  dplyr::group_by(Variety) |>
  tidyr::nest(.key = "pred")

comp <- emmeans(MOats.rg, ~ nitro | Variety) |>
  pairs() |>
  data.frame() |>
  dplyr::group_by(Variety) |>
  tidyr::nest(.key = "comp")

emmobjs <- left_join(by = "Variety", pred, comp) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    emmobj = list(emmobj(
      bhat = pred$emmean,
      se.bhat = pred$SE,
      se.diff = comp$SE,
      levels = pred$nitro,
      df = 10
    ))
  )

plots <- dplyr::group_map(
  emmobjs,
  \(x, ...)
    plot(x$emmobj[[1]], comparisons = TRUE) + ggplot2::labs(title = x$Variety)
)

patchwork::wrap_plots(plots, nrow = 3)
