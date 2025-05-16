# here i'm going to explore different ways of calculating marginal and
# conditional means using the marginaleffects package, similar to [what Andrew
# Heiss
# does](https://www.andrewheiss.com/blog/2022/11/29/conditional-marginal-marginaleffects/#marginal-effects-or-effect-of-a-variable-across-clusters-on-average)

load("./fuel_modeling_round_3.rda")
load("./sprout_modeling.rda")
load("./regen_visualize.rda")
library(marginaleffects)

# calculate rmse from predicted and actual values for a given model with
# model.frame and predict methods
rmse <- function(model) {
  actual <- model.response(model.frame(model))
  pred <- predict(model)
  sqrt(mean((pred - actual)^2))
}

rmse(m6)
rmse(m7)
rmse(mba)
mba
coef(m6)

comparisons(
  mba,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NA,
  variables = list(treat = "pairwise"),
  by = c("spp", "contrast")
) |>
  as_tibble()

comparisons(
  mba,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NULL,
  variables = list(treat = "pairwise"),
  by = c("spp", "contrast")
) |>
  as_tibble()

predictions(
  mba,
  newdata = datagrid(spp = unique, treat = unique),
  re.form = NA
) |>
  as_tibble()

predictions(
  mba,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NULL,
  by = c("spp", "treat")
) |>
  as_tibble()

get_emmobj(pred$pred[[1]], comp$comp[[1]], "treat") |> plot(comparisons = TRUE)
get_emmobj(pred$pred[[2]], comp$comp[[2]], "treat") |> plot(comparisons = TRUE)
get_emmobj(pred$pred[[3]], comp$comp[[3]], "treat") |> plot()
get_emmobj(pred$pred[[4]], comp$comp[[4]], "treat") |> plot(comparisons = TRUE)

predictions(
  m6,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NULL,
  by = c("spp", "treat")
)

predictions(
  mba,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NA,
  by = c("spp", "treat")
)

predictions(
  newdata = datagrid(spp = unique, treat = unique),
  mba,
  re.form = NA
)

predictions(
  mba,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NA
)

predictions(
  m6,
  newdata = datagrid(spp = unique, treat = unique, site = unique),
  re.form = NA
)

formula(mba)

datagrid(model = mba, spp = unique, treat = unique, site = unique)
insight::get_data(mba)

comparisons(
  mba,
  newdata = datagrid(spp = unique, treat = unique),
  re.form = NA,
  variables = list(treat = "all"),
  by = c("spp", "contrast")
  # by = TRUE
)
