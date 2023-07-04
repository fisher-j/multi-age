
d <- load2("long", treatment, all_of(load_vars)) |> 
  split(~class) |> map(~split(.x, ~treatment))
fit_exp <- map(d, ~fitdistrplus::fitdist(.x, "exp"))
walk(fit_exp, ~plot(.x))
map(d, \(x) gamlss::fitDist(x, k = 2, type = "realline")$fits) |>
  bind_rows(.id = "trt")

map(d, \(class) map(class, \(treatment) gamlss::fitDist(treatment$load, k = 2,
  type = "realline")$fits))

set.seed(123)

colony = as.factor(1:96)
colony_effect = rnorm(96, mean = 2)

field = as.factor(sort(rep(c(1:16),6)))
field_e = rnorm(16, mean = 2)
field_effect = rep(field_e, each = 6)

field_pair = as.factor(sort(rep(c(1:8),12)))
field_pair_e = rnorm(8, mean = 2)
field_pair_effect = rep(field_pair_e, each = 12)

treatment = as.factor(rep(c(rep("control", 6), rep("treat", 6)), 8))
treatment_effect = rep(c(rep(0, 6), rep(1, 6)), 8)     

response = treatment_effect + field_effect + field_pair_effect + colony_effect
df1 = data.frame(treatment, field_pair, field, colony, response)
df1
