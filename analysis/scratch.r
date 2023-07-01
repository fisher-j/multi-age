
d <- load2("long", treatment, all_of(load_vars)) |> 
  split(~class) |> map(~split(.x, ~treatment))
fit_exp <- map(d, ~fitdistrplus::fitdist(.x, "exp"))
walk(fit_exp, ~plot(.x))
map(d, \(x) gamlss::fitDist(x, k = 2, type = "realline")$fits) |>
  bind_rows(.id = "trt")

map(d, \(class) map(class, \(treatment) gamlss::fitDist(treatment$load, k = 2,
  type = "realline")$fits))

