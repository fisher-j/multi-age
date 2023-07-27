
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


#########################################################
######## Am I specifying the correct model? #############
#########################################################

form <- load ~ treatment + (1 | site:treatment:corner) + (1 | site)
amod <- lme4::lmer(form, dd$onehr, REML = FALSE)
summary(amod)
form2 <- load ~ 1 + (1 | site/treatment/corner)
nmod <- lme4::lmer(form2, dd$onehr, REML = FALSE)
summary(nmod)
res <- pbkrtest::PBmodcomp(amod, nmod)
summary(res)
res2 <- pbkrtest::KRmodcomp(amod, nmod)
res2
pluck(tmod, summary, "test")
pluck(tmod2, "test", \(x) `[`(x, 1, ))


# Using NLME, gives an estimated DF
m2 <- nlme::lme(
  fixed = load ~ treatment,
  random = ~ 1 | site/treatment/corner,
  data = dd$onehr
)
summary(m2)

m3 <- aov(load ~ treatment, dd$onehr)
anova(m3)

m4 <- lmerTest::lmer(form, dd$onehr, REML = FALSE)
anova(m4)

amod <- lme4::lmer(form, dd$onehr, REML = TRUE)
ggplot2::stat_qq

# is no-intercept the same as yes intercept?
d1 <- d[1,] |> unnest(data)
form <- load ~ treatment + (1 | site/treatment/corner)
mt <- lme4::lmer(load ~ treatment + (1 | site/treatment/corner), d1)
mt2 <- lme4::lmer(load ~ 0 + treatment + (1 | site/treatment/corner), d1)
emmeans::emmeans(mt, ~ treatment)
emmeans::emmeans(mt2, ~ treatment)
broom.mixed::tidy(mt, effects="ran_pars")$estimate |> sum()
broom.mixed::tidy(mt2, effects="ran_pars")
# Yes it seems like it is


brms::bf(load + 0 ~ treatment) + lf()

posterior_expected_predicitons <- function(data, plot = TRUE) {
  newdata <- expand(data$data[[1]], nesting(treatment))
  data <- data |>
    mutate(
      pred = list(
        tidybayes::epred_draws(mod, newdata, re_formula = NA, value = "pred")),
      )
  if (plot) {
    p <- data |> unnest(pred) |>
      ggplot(aes(pred, treatment)) +
      tidybayes::stat_halfeye(normalize = "panels") +
      facet_wrap(~class, scales = "free_x")
    print(p)
  }
  invisible(data)
}

library(tidybayes)
library(brms)
m1 <- bf2$mod[[5]]
m1
m2 <- bf3$mod[[1]]
m2
pp_check(m1)
pp_check(m2)

get_variables(m1)
get_variables(m2)

tidybayes::add_fitted_draws

stancode(m1)
summary(m1)
summary(m2)

##############################################################################
########################## Posterior predictive check ########################
##############################################################################

# Add predictions of observations (transects)
# also add quantile for setting limits on facet coordintates

# Posterior predictive check of one model
posterior_predictive_check <- function(models) {
  models <- mutate(models,
    pred = list(tidybayes::add_predicted_draws(data, mod, ndraws = 15)),
    lims = list(tibble(
      xmin = min(
        quantile(data$load, .0001),
        quantile(pred$.prediction, .0001)
      ),
      xmax = max(
        quantile(data$load, .9999),
        quantile(pred$.prediction, .9999)
      ))
    )
  )
  ggplot() + 
  geom_line(
    data = unnest(models, data),
    aes(x = load),
    stat = "density", size = 1
  ) +
  geom_line(
    data = unnest(models, pred),
    aes(x = .prediction, group = .draw),
    stat = "density", alpha = 0.15, size = 1
  ) +
  facet_wrap(~class, scales = "free") +
  coord_cartesian_panels(
    panel_limits = unnest(select(models, lims), lims)
  )
}

posterior_predictive_check(bf3)

source("./scripts/coord_cartesian_panels.r")
bf3 |> mutate(
  pred = list(
    tidybayes::epred_draws(mod, expand(data, nesting(treatment)), re_formula = NA, value = "pred") |>
    tidybayes::compare_levels(pred, by = treatment) |>
    select(contrast = treatment, pred) 
  ),
  lims = list(tibble(xmin = quantile(pred$pred, .001), xmax = quantile(pred$pred, .999)))
) |> unnest(lims)

# get posterior predictions for treatments, ignoring random effects. These are
# predictions for the expected value across all sites.
posterior_expected_predicitons <- function(data, plot = TRUE) {
  newdata <- expand(data$data[[1]], nesting(treatment))
  data <- mutate(data,
    pred = list(
      tidybayes::epred_draws(mod, newdata, re_formula = NA, value = "pred")
    ),
    lims = list(
      tibble(xmin = 0, xmax = quantile(pred$pred, .999))
    )
  )
  if (plot) {
    p <- data |> unnest(pred) |>
      ggplot(aes(pred, treatment)) +
      tidybayes::stat_halfeye(normalize = "panels") +
      facet_wrap(~class, scales = "free_x") +
      coord_cartesian_panels(panel_limits = unnest(select(data, lims), lims))
    print(p)
  }
  invisible(data)
}

posterior_expected_predicitons(bf3)
