
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

field = as.factor(sort(rep(all(1:16),6)))
field_e = rnorm(16, mean = 2)
field_effect = rep(field_e, each = 6)

field_pair = as.factor(sort(rep(all(1:8),12)))
field_pair_e = rnorm(8, mean = 2)
field_pair_effect = rep(field_pair_e, each = 12)

treatment = as.factor(rep(all(rep("control", 6), rep("treat", 6)), 8))
treatment_effect = rep(all(rep(0, 6), rep(1, 6)), 8)     

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

pars <- tidybayes::get_variables(bf3$mod[[5]]) |>
  str_match("^b|^sd|shape|hu")

bayesplot::mcmc_parcoord(bf3$mod[[4]], alpha = 0.05, np = bayesplot::nuts_params(bf3$mod[[4]]), regex_pars = "^b|^sd|shape|hu")

######################################################################
######################## diagnostic functions ########################
######################################################################

show_pairs <- function(obj, class, pars, mod) {
  mod <- filter(obj, class == .env$class) |> pluck(mod, 1)
  n_p <- bayesplot::nuts_params(mod)
  bayesplot::mcmc_pairs(mod, np = n_p, regex_pars = pars)
}

show_parameters <- function(obj, class, pars = "^b|^sd|shape|hu", mod) {
  mod <- filter(obj, class == .env$class) |> pluck(mod, 1)
  n_p <- bayesplot::nuts_params(mod)
  bayesplot::mcmc_parcoord(mod, np = n_p, regex_pars = pars)
}

count_divergent <- function(obj, mod) {
  transmute(obj, 
    n_divergent = nrow(filter(
      brms::nuts_params(.data[[mod]]), 
      Parameter == "divergent__" & Value == 1
    ))
  )
}

######################################################################

show_pairs(bf3, "dufflitter", "^b", "prior_mod")

mod <- filter(bf3, class == "dufflitter") |> pluck("mod", 1)
n_p <- brms::nuts_params(mod)
bayesplot::mcmc_scatter(mod, np = n_p, pars = )

plot(bf3$mod[[4]])

bf4 <- mutate(bf3,
  mod = list(
    update(mod, cores = 4, iter = 6000, warmup = 5000,
      control = list(adapt_delta = 0.99)
    )
  )
)

tidybayes::get_variables(bf4$mod[[1]])
count_divergent(bf3, "mod")
show_parameters(temp, "hundhr", mod = "mod_prior")
show_pairs(temp, "hundhr", "b_treatmentgs|sd", "mod_prior")

plot(bf4$mod[[4]])
summary(bf4$mod[[4]])

sprintf("%.2f, %.2f", )
all(pi, 2*pi)


sprintf("%s %d", "test", 1:3)

brms::prior(lognormal(5.053056, 0.09562761)) %>% 
  tidybayes::parse_dist() %>% 
  ggplot(aes(xdist = .dist_obj, y = prior)) + 
  tidybayes::stat_halfeye(.width = all(.5, .95), p_limits = all(.0001, .9999)) +
  scale_y_discrete(NULL, breaks = NULL, expand = expansion(add = 0.1)) +
  labs(title = "Lognormal(5.053056, 0.09562761)",
       subtitle = "A normal prior on the log link is like a lognormal prior on the identity link.",
       x = expression(exp(italic(p)(beta[0])))) +
  coord_cartesian(xlim = all(100, 250))



par(mfrow = all(1, 2))
curve(dnorm(x, 43, 65), from = 0, to = 200)
curve(dlnorm(x, 3.323, 0.6), from = 0, to = 200)

quantile(rlnorm(150, 3.323, 1.029), all(0.025, 0.975))

draws <- rnorm(1e6, 43, 65)
draws <- draws[draws > 0]
lnorm_param(draws)
par(mfrow = all(1, 2))
curve(pnorm(x, 43, 65), from = 0, to = 200)
curve(plnorm(x, 3.6, 1.15), from = 0, to = 200)

par(mfrow = all(1, 2))
curve(dnorm(x, 43, 65), from = 0, to = 200)
curve(dlnorm(x, 4.1, 0.9), from = 0, to = 200)

length(draws)
quantile(draws, all(0.025, 0.975))
mean(draws)
sd(draws)

# trying to figure out the right distribution for prior with log scale
log(71 / sqrt(47^2 / 71^2 + 1))
sqrt(log(47^2 / 71^2 + 1))
a <- rlnorm(1e6, 3.323, 1.029)
mean(a)
sd(a)
sd(bf3$data[[4]]$load) * 2.5
mean(bf3$data[[4]]$load)
mean(bf3$data[[3]]$load)
mean(bf3$data[[6]]$load)
sd(bf3$data[[6]]$load) * 2.5

count_divergent(temp, "mod_prior")
brms::prior_summary(temp$mod[[4]])
temp$mod[[3]]
mean(temp$data[[3]]$load == 0)


curve(dbeta(x, 1,1), 1, 10)

# Student t distribution with flexible location and scale
my_t <- function(N, nu, mu, sd) {
  x1 <- rt(N, nu) # 1
  x2 <- x1/sqrt(nu/(nu-2)) # 2
  x3 <- x2 * sd # 3
  x4 <- x3 + mu # 4
  return(x4)
}

a <- rlnorm(1e5, 4.1, 0.6)
plot(density(log(a)))
plot(density(a))
plot(density(rnorm(1e5, 4.1, 0.6)))
plot(density(exp(rnorm(1e5, 4.1, 0.6))))
tt <- exp(my_t(1e5, 3, -0.7, 1))
median(tt)
lnorm_param(d$data[[1]]$load)
plot(density(my_t(1e5, 3, -0.7, 1)), xlim = c(-5, 3))

hist(tt)
plot(density(log(rnorm(1e5, 4.1, 0.6))))

sum(rexp(1e5, 1 / 47) == 0)
hist((rexp(1e5, 1 / 47)))
pexp(0, 1/47)
plnorm(0, 4.1, 0.6)

log(all(47, 71))
a <- rnorm(1e5, 3.8, 4.2)
a <- a[a > 0]
plot(density(exp(a)))


m1$mod[[1]]
bf2$mod[[1]]
bf3$mod[[1]]
brms::standata(bf3$mod[[1]])
brms::stancode(bf3$mod[[1]])
summary(bf3$mod[[1]])

brms::ranef(bf3$mod[[1]])
coef(bf3$mod[[1]])

# camp6 GS

exp(-.7)

bf3$data[[1]][1:3,]
# 1 camp6 gs        e        225 1.96
# 2 camp6 gs        e        315 0.850
# 3 camp6 gs        n        135 0.673

coef(bf3$mod[[1]])$site[1, 1, 1, drop = F]
coef(bf3$mod[[1]])$`site:treatment`[1, 1, 1, drop = F]
coef(bf3$mod[[1]])$`site:treatment:corner`[1, 1, 1, drop = F]
exp(-.7 + .2629 + .2513 + .0606)
exp(-0.6999828 + 0.262 + 0.2512 + 0.0606)
brms::posterior_epred(bf3$mod[[1]])[ ,1] |> exp() |> sd()


################################################################################
############################# compare some models ##############################
################################################################################

# treatment as random, intercept specified with prior
m2<-brms::posterior_summary(bf4$mod[[1]], variable = c("b_Intercept", "r_treatment"))
m2 <- exp(m2[-1,1] + m2[1,1]) |> as_tibble(rownames = "treatment") |> rename(bf4 = 2)

all <- unnest(d, data) |> 
  filter(class == "onehr") |>
  group_by(treatment) |>
  summarize(mean = mean(load))

# ill specified model?
m1 <- brms::posterior_summary(bf3$mod[[1]])[1:4, 1] |> exp() |> as_tibble(rownames = "treatment") |> rename(bf3 = 2)

# Here I use the default prior which is student_t(3, -0.3, 2.5)
# this gives slightly higher estimates all around
m3<-brms::posterior_summary(bf5$mod[[1]], variable = c("b_Intercept", "r_treatment"))
m3 <- exp(m3[-1,1] + m3[1,1]) |> as_tibble(rownames = "treatment") |> rename(bf5 = 2)

cbind(all, m1[2], m2[2], m3[2])

brms::prior_summary(bf5$mod[[1]])
pluck(d, "data", 1, "load") |> log() |> mean()



##############################################################################
############################# Interesting plots ############################## 
##############################################################################

# Look at coefficients currently for bf4
bf4$mod[[1]] |>
  bayesplot::mcmc_intervals(
    regex_pars = c("^r_site__a", "^r_site:treatment__a", "^b", "^sd")
    # transformations = "exp"
  )

# plot variance of each group
brms::as_draws_df(bf4$mod[[1]]) |>
  pivot_longer(starts_with("sd")) |>
  ggplot(aes(x = value, fill = name)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 2/3) +
  scale_fill_manual(values = c("tan4", "tomato1", "thistle", "steelblue3")) +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle(expression(sigma["<group>"])) +
  coord_cartesian(xlim = c(0, 4))
################################################################################

# Grand mean of data
d$data[[1]]$load |> mean()

# Grand mean of model
as_draws_df(bf4$mod[[1]]) |> 
  transmute(alpha = b_a_Intercept) |>
  mean_qi() |>
  mutate(across(1:3, exp))

######## building up ########
m1 <- glm(load ~ 0 + treatment + site, d$data[[1]], family = Gamma(link = "log"))
exp(coef(m1))
m2 <- glm(load ~ 0 + treatment + site, d$data[[1]], family = Gamma(link = "log"))
exp(coef(m2))
library(emmeans)
emmeans(m2, ~ treatment, type = "response")
library(lme4)
m3 <- glmer(
  load ~ 0 + treatment + (1 | site), d$data[[1]], family = Gamma(link = "log"))
fixef(m3) |> exp()
emmeans(m3, ~ treatment, type = "response")
m4 <- glmer(
  load ~ 0 + treatment + (1 | site/treatment ), d$data[[1]], family = Gamma(link
    = "log"))
fixef(m4) |> exp()
emmeans(m4, ~ treatment, type = "response")
#############################

# mean of data by treatment
d$data[[1]] |> 
  group_by(treatment) |>
  summarize(load = mean(load))

as_draws_df(bf4$mod[[1]]) |> 
  select(starts_with("b_b_treatment"), b_a_Intercept) |>
  pivot_longer(
    starts_with("b_b_treatment"),
    names_transform = \(x) str_remove(x, "b_b_treatment")) |>
  mutate(mu = value + b_a_Intercept) |>
  group_by(name) |>
  mean_qi(mu) |> mutate(across(2:4, exp))

as_draws_df(bf5$mod[[1]]) |>
  select(matches("^r_treatment")) |>
  pivot_longer(everything()) |>
  mutate(name = str_extract(name, "gs|ha|hd|ld")) |>
  group_by(name) |>
  mean_qi(value) |> mutate(across(2:4, exp))


# I'm going to try to start small and build up, I want to see what the priors
# look like, I'm working through section 11.1 of SK's version of rethinking.
#
# This prior (sd = 1) is about 1.5 sds of the raw data

treatprior <- "normal(0, 1)"
bt1 <- mutate(d[1,],
  priors = list(
    set_prior(
      str_glue("normal({mu}, {sigma})", .envir = lnp(data$load)),
      nlpar = "a"
    ) +
    set_prior(treatprior, nlpar = "b", coef = "treatmentgs") +
    set_prior(treatprior, nlpar = "b", coef = "treatmentha") +
    set_prior(treatprior, nlpar = "b", coef = "treatmenthd") +
    set_prior(treatprior, nlpar = "b", coef = "treatmentld")
  ),
  mod = list(brm(
    bf(
      load ~ a + b, a ~ 1, b ~ 0 + treatment, nl = TRUE
    ),
    data = data,
    family = brms::hurdle_gamma(),
    prior = priors,
    sample_prior = TRUE,
    warmup = 3000,
    iter = 4000,
    cores = 4,
    control = list(adapt_delta = 0.95)
    # file = paste0("fits/bf4_", class)
  ))
)

# This is how I can check the prior difference between treatments
prior <- prior_draws(bt1$mod[[1]]) |>
  mutate(
    p1 = exp(b_a + b_b_treatmentgs),
    p2 = exp(b_a + b_b_treatmentha),
    diff = abs(p1 - p2)
  )
prior |>
  ggplot(aes(x = diff)) +
  geom_density(linewidth = 0, alpha = 3/4, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(
    title = treatprior,
    x = "prior diff between treatments"
  )
  # coord_cartesian(xlim = c(0, 5))


# This is how I simulate prior draws for this model
a <-  rnorm(1e6, -.73, 1.1)
b1 <- rnorm(1e6, 0, 1)
b2 <- rnorm(1e6, 0, 1)
diff = abs(exp(a + b1) - exp(a + b2))
mean(diff)
quantile(diff)
tibble(diff = diff) |>
  ggplot(aes(x = diff)) +
  geom_density(linewidth = 0, adjust = 0.1) +
  scale_y_continuous(NULL, breaks = NULL) +
  # coord_cartesian(xlim = c(0, 7)) +
  ggtitle("prior diff")

# emperical and modeled grand mean
mean(bt1$data[[1]]$load) #0.876
as_draws_df(bt1$mod[[1]]) |> 
  transmute(alpha = exp(b_a_Intercept)) |>
  mean_qi() 
# alpha .lower .upper .width .point .interval
#   <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1 0.857  0.316   1.84   0.95 mean   qi


# emperical and modeled conditional means
d$data[[1]] |> group_by(treatment) |>
  summarize(mean = mean(load))
#   treatment  mean
#   <chr>     <dbl>
# 1 gs        0.598
# 2 ha        1.20
# 3 hd        1.04
# 4 ld        0.668


as_draws_df(bt1$mod[[1]]) |>
  pivot_longer(starts_with("b_b_treatment")) |>
  mutate(
    name = str_extract(name, "gs|ha|hd|ld"),
    mean = exp(b_a_Intercept + value)
  ) |>
  group_by(name) |>
  mean_qi(mean)

#   name   mean .lower .upper .width .point .interval
#   <chr> <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>
# 1 gs    0.610  0.474  0.784   0.95 mean   qi
# 2 ha    1.22   0.947  1.58    0.95 mean   qi
# 3 hd    1.05   0.809  1.35    0.95 mean   qi
# 4 ld    0.682  0.532  0.881   0.95 mean   qi


as_draws_df(bf5$mod[[1]]) |>
  pivot_longer(starts_with("r_treatment")) |>
  mutate(
    name = str_extract(name, "gs|ha|hd|ld"),
    mean = exp(b_Intercept + value)
  ) |>
  group_by(name) |>
  mean_qi(mean)

# Look at coefficients currently
get_variables(bf5$mod[[1]])
bf5$mod[[1]] |>
  bayesplot::mcmc_intervals(
    regex_pars = c("^r_treatment", "^r_site\\[", "^r_site:treatment\\[", "^b", "^sd")
    # transformations = "exp"
  )

# Grand mean of model
as_draws_df(bf5$mod[[1]]) |> 
  transmute(alpha = exp(b_Intercept)) |>
  mean_qi()

# this shows the difference between the sample means and the models with
# variable, and fixed treatment effects.
plotdat <- list()
plotdat$bf5 <- as_draws_df(bf5$mod[[1]]) |>
  transmute(
    across(starts_with("r_treatment"), \(x) exp(x + b_Intercept))
  ) |>
  pivot_longer(everything(), values_to = "load") |>
  mutate(treatment = str_extract(name, "gs|ha|hd|ld")) |>
  group_by(treatment) |>
  mean_qi(load)
plotdat$bf4 <- as_draws_df(bf4$mod[[1]]) |>
  transmute(
    across(starts_with("b_b_treatment"), \(x) exp(x + b_a_Intercept))
  ) |>
  pivot_longer(everything(), values_to = "load") |>
  mutate(treatment = str_extract(name, "gs|ha|hd|ld")) |>
  group_by(treatment) |>
  mean_qi(load)
plotdat$sample <- d$data[[1]] |>
  group_by(treatment) |>
  mean_qi(load)
graphics <- aes(treatment, load, ymin = .lower, ymax = .upper, color = group)
p5 <- ggplot(bind_rows(plotdat, .id = "group"), graphics) +
geom_pointrange(position = position_dodge(0.4)) +
geom_hline(yintercept = d$data[[1]]$load |> mean(), color = "gray50")

p5 + ggtitle("intercept, all~s_t(3, 0, 2)")
p4 + ggtitle("intercept, treat~s_t(3, 0, 1), sd~s_t(3, 0, 1)")
p3 + ggtitle("intercept, treatment~n(0, 1), sd~s_t(3, 0, 0.25)")
p2 + ggtitle("intercept, sd~s_t(3, 0, 2.5)")
p1 + ggtitle("wrong prior")

predict_expected_contrasts(bf4, 0)
predict_expected_contrasts2(bf5, 0)

get_variables(bf4$mod[[1]])
prior_summary(bf4$mod[[1]])

intercept <- rnorm(1e5, -0.733, 1.0962)
treatment <- rnorm(1e5, 0, 1)
ran_sd <- 1
site <- my_t(1e5, 3, 0, ran_sd)
plot <- my_t(1e5, 3, 0, ran_sd)
corner <- my_t(1e5, 3, 0, ran_sd)
res <- exp(intercept + treatment + site + plot + corner)
plot(density(res), xlim = c(0, 7))
mean(res)
quantile(res, c(.05, .5, .95))

prior_summary(bf4$mod[[1]])
stancode(bf4$mod[[1]])
post <- posterior_samples(bf4$mod[[1]])
str(post)
bf4$mod[[1]]
prior_summary(bf4$mod[[1]])

################################################################################
############################### prior prediction ###############################
################################################################################

# Student t distribution with flexible location and scale
my_t <- function(N, nu, mu, sd) {
  x1 <- rt(N, nu) # 1
  x2 <- x1/sqrt(nu/(nu-2)) # 2
  x3 <- x2 * sd # 3
  x4 <- x3 + mu # 4
  return(x4)
}

student_t_plus <- function(N, nu, mu, sd) {
  N3 <- N*3
  y <- ggdist::rstudent_t(N3, nu, mu, sd)
  y <- y[y > 0]
  y <- sample(y, N)
}

rnorm_plus <- function(N, mu, sd) {
  N3 <- N*3
  y <- rnorm(N3, mu, sd)
  y <- y[y>0]
  sample(y, N)
}

prior_sim <- function() {
  N <- 1e4
  M <- -0.73
  S <- 1.1
  a_bar <- rnorm(N, M, S)
  U <- rnorm(N, 0, 1)
  V <- rnorm(N, 0, 1)
  W <- rnorm(N, 0, 1)
  sigma_rand <- 1
  sigma_u <- rnorm_plus(N, 0, 1)
  sigma_v <- rnorm_plus(N, 0, 1)
  sigma_w <- rnorm_plus(N, 0, 1)
  beta <- rnorm(N, 0, 1)
  shape <- rgamma(N, 0.01, 0.01)
  hu <- rbeta(N, 1, 1)
  mu <- exp(a_bar + U*sigma_u + V*sigma_v + W*sigma_w + beta)
  post <- rgamma(N, shape, shape / mu)
}

summary(mu)
hist(mu)
map(1:15, prior_sim)
sum(mu < 0.1)
plot(density(post), xlim = c(0, 200))
lines(density(post, adjust = 3))
posterior::summarize_draws(bf4a$mod[[1]]) |>
  filter(str_detect(variable, "prior_b_a|prior_b_b|prior_sd_site$"))
posterior::summarize_draws(cbind(a_bar, beta, sigma_u))

#### Plot prior vs. posterior distributions ####
get_variables(m)

mod <- bf4$mod[[1]]


# TODO: Compare this model to current model bf4 to assess the much wider priors

bf4a <- mutate(d,
  priors = list(
    set_prior(
      str_glue("normal({mu}, {sigma})", .envir = lnp(data$load)),
      nlpar = "a", coef = "Intercept"
    ) +
    set_prior("normal(0, 1)", nlpar = "a", class = "sd") +
    set_prior("normal(0, 1)", nlpar = "b", coef = "treatmentgs") +
    set_prior("normal(0, 1)", nlpar = "b", coef = "treatmentha") +
    set_prior("normal(0, 1)", nlpar = "b", coef = "treatmenthd") +
    set_prior("normal(0, 1)", nlpar = "b", coef = "treatmentld")
  ),
  mod = list(brms::brm(
    brms::bf(
      load ~ a + b,
      a ~ 1 + (1 | site/treatment/corner),
      b ~ 0 + treatment,
      nl = TRUE),
    data,
    warmup = 4000,
    iter = 5000,
    cores = 4,
    control = list(adapt_delta = 0.99),
    family = brms::hurdle_gamma(),
    prior = priors,
    sample_prior = TRUE,
    file = paste0("fits/bf4a_", class)
  ))
)

####### Balance divergent transitions with low effective sample size ########
bf4a <- mutate(bf4a,
  mod = list(update(mod,
    file = paste0("fits/bf4a_", class),
    control = list(adapt_delta = 0.99),
    warmup = 4000,
    iter = 5000,
    cores = 4
  ))
)

bf4 <- mutate(bf4,
  mod = list(update(mod,
    file = paste0("fits/bf4_", class),
    control = list(adapt_delta = 0.95),
    warmup = 4000,
    iter = 5000,
    cores = 4
  ))
)
#############################################################################


plot_pri_post_fixed(bf4$mod[[1]])
plot_pri_post_sd(bf4$mod[[1]])
plot_pri_post_sd(bf4a$mod[[1]])
plot_pri_post_fixed(bf4a$mod[[1]])
predict_posterior_expected(bf4)
predict_expected_contrasts(bf4, 0)

####################### expose stan function #########################
# and compare to other methods
# <https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html>

library(rstan)

model_code <- "
  functions {
    real stan_student_t_rng() {
      return student_t_rng(3, 2, 2);
    }
  }
  model {} 
"
expose_stan_functions(stanc(model_code = model_code))

y1 <- modified_t(1e4, 3, 2, 2)
y2 <- ggdist::rstudent_t(1e4, 3, 2, 2)
y3 <- sapply(numeric(1e4), \(x) stan_student_t_rng())

l1 <- list(
modified_t = quantile(y1, c(0.05, 0.5, 0.95)),
rstudent_t = quantile(y2, c(0.05, 0.5, 0.95)),
stan_student_t_rng = quantile(y3, c(0.05, 0.5, 0.95))
)
do.call(rbind, l1)

#######################################################################


d |>
  mutate(
    prior_dist = list(tidybayes::parse_dist(priors) |> 
    rename(dist_class = class))
  ) |>
  unnest(prior_dist) |>
    ggplot(aes(ydist = .dist_obj, color = coef)) +
    stat_slab(fill = NA)

library(ggdist)
brms::set_prior("student_t(3, 0, 2)", class = "sd") |>
tidybayes::parse_dist()
ggplot(aes(ydist = .dist_obj)) +
stat_slab()

temp <- bf4a$mod[[1]]

tidy_draws(temp) |> 
select(starts_with("prior")) |>
rename_with(~ str_remove(.x, "prior_"))
predict.brmsfit

mod <- prior_only_bf4a$mod[[1]]
dd <- prior_only_bf4a$data[[1]]
pp_check(dd$load, posterior_predict(mod, ndraws = 15), ppc_dens_overlay)
coord_cartesian(xlim = c(0, 10), ylim = c(0, 1))

summary(dd)
hist(dd$load)
plot(density(dd$load))
summary(t(posterior_predict(mod)))


prior_predictive_check <- function(models) {
  # models <- bf4a
  models <- mutate(models,
    newdata = list(select(tidy_draws(mod), starts_with("prior")) |>
      rename_with(~ str_remove(.x, "prior_"))
  ))
  models <- mutate(models,
    pred = list(tidybayes::add_predicted_draws(newdata, mod, ndraws = 15)),
    lims = list(tibble(
      xmin = min(
        quantile(data$load, .001),
        quantile(pred$.prediction, .001)
      ),
      xmax = max(
        quantile(data$load, .999),
        quantile(pred$.prediction, .999)
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
  facet_wrap(~class, scales = "free", labeller = fuel_class_labeller) +
  coord_cartesian_panels(
    panel_limits = unnest(select(models, lims), lims)
  )
}
prior_predictive_check(bf4a)

summary(bf4a$mod[[1]])$fixed
summary(bf4a$mod[[1]])$random

rhat(bf4a$mod[[1]]$fit)
neff_ratio(bf4a$mod[[1]]$fit)
summary(bf4a$mod[[1]])
rstan::summary.stanfit(bf4a$mod[[1]])


# summarize draws for selected variables
as_draws_df(bf4a$mod[[1]]) |>
  select(starts_with(c("b_", "sd_")), shape, hu) |>
  posterior::summarize_draws(
    "mean", ~posterior::quantile2(.x, c(0.05, .5, .95)), "rhat", "ess_bulk",
    "ess_tail"
  )

sprouts |>
  mutate(
    id = paste(Site, Plot, Trtmt),
    ht_inc_1 = round((HT5yr_m - HT1yr_m) / 4, 3),
    ht_inc_2 = round((HT10yr_m - HT5yr_m) / 5, 3),
    matches = (ht_inc_1 == round(`HTI1-5yr`, 3)) & (ht_inc_2 == round(`HTI5-10yr`, 3)),
  ) |>
  select(id, matches("ht"), -HT10rank, matches) |>
  filter(matches)

library(marginaleffects)
library(patchwork)
library(lme4)

dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)

mod <- lmer(mpg ~ hp + am + (0 + hp | cyl), data = dat)

predictions(mod, newdata = "mean", vcov = "kenward-roger")


d
d$plot <- factor(d$plot)
d$spp <- factor(d$spp)
m9 <- lmer(
  ht ~ treat * spp * year + (0 + spp | plot) + (0 + spp | tree),
  data = d
)

predictions(m9, newdata = "mean", vcov = "kenward-roger")

# Test residuals with DHARma
res_sim <- simulateResiduals(mht9, re.form = NA)
res_sim <- simulateResiduals(mht9, re.form = NULL)
plot(res_sim)
plotResiduals(res_sim, form = dht$year)
plotResiduals(res_sim, form = with(dht, paste(spp, treat)))
plotResiduals(res_sim, form = dht$treat)

recalculateResiduals(res_sim, group = dht$plot) |> plot()
recalculateResiduals(res_sim, group = dht$tree) |> plot(quantreg = FALSE)

plot(predict(mht9), resid(mht9, type = "pearson"))
plot(predict(mht9f), resid(mht9f, type = "pearson"))
plot(exp(predict(mht9f, re.form = NA)), resid.merMod(mht9f, type = "pearson"))
abline(h = 0)

# Maybe I should use a gamma model

mht9a <- glmer(
  ht ~ treat * spp * year + (1 | plot) + (1 | tree),
  data = mutate(dht, year = scale(year)), family = Gamma(link = log),
  control = glmerControl(optCtrl = list(maxfun = 1e6))
)

mht9b <- glmer(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year + (1 | plot) + 
  (1 | tree),
  data = mutate(dht, year = scale(year)), family = Gamma(link = log),
  control = glmerControl(optCtrl = list(maxfun = 1e6))
)

mht9a_all <- allFit(mht9a)
summary(mht9a_all)

summary(mht9a)

# What about glmmTMB?
library(glmmTMB)

mht11 <- glmmTMB(
  ht ~ treat * spp * year + (1 | plot) + (1 | tree),
  data = dht, family = Gamma(link = "log")
)

mht12 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year + (1 | plot) + 
  (1 | tree),
  data = dht, family = Gamma(link = "log")
)

mht13 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year + 
  (0 + spp | plot) + (0 + spp | tree),
  data = dht, family = Gamma(link = "log")
)

mht14 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year +
  (1 | plot) + (1 | tree),
  data = mutate(dht, year = factor(year)), family = Gamma(link = "log")
)

mht15 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year +
  (1 | plot) + (1 | tree),
  dispformula = ~ year,
  data = mutate(dht, year = factor(year)), family = Gamma(link = "log")
)

mht16 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year +
  (1 | plot) + (1 | tree),
  dispformula = ~ year * spp,
  data = mutate(dht, year = factor(year)),
  family = Gamma(link = "log")
)

mht17 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year +
  (1 | plot) + (1 | tree),
  dispformula = ~ year * spp + treat + treat:spp,
  data = mutate(dht, year = factor(year)), family = Gamma(link = "log")
)

mht18 <- glmmTMB(
  ht ~ treat + spp + year + treat:spp + treat:year + spp:year +
  (1 | plot) + (1 | tree),
  data = mutate(dht, year = factor(year)),
  family = gaussian(link = "log")
)

AIC(mht11, mht12, mht13, mht14, mht15, mht16, mht17, mht18)

grid <- emmeans(
  mht18,
  pairwise ~ treat,
  by = c("spp"),
  at = list(year = "10"),
  type = "response"
)

grid_cld <- cld(grid, Letters = letters) |> as_tibble()

multiple_years_plot2 <- filter(dht, year == 10) |>
  ggplot(aes(treat, ht, color = spp, fill = spp)) +
  facet_grid(~spp, switch = "x") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        ) +
  geom_dots() +
  scale_color_brewer(palette = "Set2", aesthetics = c("color", "fill")) +
  geom_pointrange(
    data = grid_cld,
    aes(y = response, ymin = lower.CL, ymax = upper.CL),
    color = "gray50", position = position_nudge(x = -0.07),
    size = 0.7,
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = grid_cld,
    aes(y = upper.CL, label = .group),
    color = "black",
    position = position_nudge(x = -0.3, y = 0.2)
  ) +
  labs(x = "Treatment", y = "Height (m)", color = "Species", fill = "Species") +
  theme(legend.position = "bottom")

multiple_years_plot2

# Test residuals with DHARma
res_sim <- simulateResiduals(mht9i, re.form = NULL)
plot(res_sim)


res_group <- recalculateResiduals(res_sim, group = with(dht, interaction(spp, treat, year)))
plot(res_group)
plotResiduals(res_sim, form = with(dht, interaction(year, spp)))
plotResiduals(res_sim, form = with(dht, interaction(spp, treat)))
plotResiduals(res_sim, form = with(dht, interaction(year, treat)))
plotResiduals(res_sim, form = dht$treat)

testDispersion(res_sim)


# compare gamma and 10 year only models
standard_model_col_names <- c(
  LCL = "lower.CL",  UCL = "upper.CL",  LCL = "asymp.LCL", UCL = "asymp.UCL",
  emmean = "response"
)

list("year 10 only" = mht108cld, "multi-year" = mht9cld) |>
  map(\(x) rename(x, any_of(standard_model_col_names))) |>
  bind_rows(.id = "model") |>
  mutate(
    .group = str_replace_all(.group, " ", "")
  ) |>
  ggplot(aes(treat, ht, color = model, fill = model)) +
  facet_grid(~spp) +
  geom_pointrange(
    aes(y = emmean, ymin = LCL, ymax = UCL),
    position = position_dodge(width = 0.4),
    size = 0.7,
    linewidth = 1
  ) +
  geom_dots(
    data = dht10, color = "gray50", fill = "gray70",
    position = position_nudge(x = 0.2), scale = 0.7, binwidth = 0.18
  ) +
  geom_text(
    aes(y = UCL + 0.6, label = .group, color = model,),
    position = position_dodge(width = 0.4),
  ) +
  theme(legend.position = "bottom") + 
  scale_color_manual(
    values = rnd_color_brewer("Set2", c(1,4)),
    aesthetics = c("color", "fill")
  )

# multi-year, observed vs simulated
for (n in 1:14) { print(
sprouts |>
  lengthen_data("ht") |>
  select(treat, spp, year, observed = ht) |>
  mutate(predicted = pull(simulate(mht9i, type = "response", re.form = NULL))) |>
  filter(year == 10) |>
  pivot_longer(c(observed, predicted), names_to = "type", values_to = "ht") |>
  ggplot(aes(x = ht, y = treat, color = type, fill = type)) +
  stat_slab(normalize = "panels", alpha = 0.3) +
  facet_grid(spp ~ year, labeller = label_both) +
  labs(x = "Height (m)", y = "Treatment", title = "Multi-year model simulation") +
  scale_color_manual(
    values = rnd_color_brewer("Set2", c(1,4)),
    aesthetics = c("color", "fill")
  ) +
  scale_x_continuous(limits = c(0, 25))
) }

sprouts |>
  lengthen_data("ht") |>
  select(treat, spp, year, observed = ht) |>
  mutate(predicted = predict(mht9i, type = "response", re.form = NULL)) |>
  filter(year == 10) |>
  pivot_longer(c(observed, predicted), names_to = "type", values_to = "ht") |>
  ggplot(aes(x = ht, y = treat, color = type, fill = type)) +
  stat_slab(normalize = "panels", alpha = 0.3) +
  facet_grid(spp ~ year, labeller = label_both) +
  labs(x = "Height (m)", y = "Treatment", title = "Multi-year model simulation") +
  scale_color_manual(
    values = rnd_color_brewer("Set2", c(1,4)),
    aesthetics = c("color", "fill")
  ) +
  scale_x_continuous(limits = c(0, 25))

# 10-year, observed vs simulated
sprouts |>
  lengthen_data("ht") |>
  filter(year == 10) |>
  select(treat, spp, year, observed = ht) |>
  mutate(predicted = pull(simulate(mht104, re.form = NULL))) |>
  pivot_longer(c(observed, predicted), names_to = "type", values_to = "ht") |>
  ggplot(aes(x = ht, y = treat, color = type, fill = type)) +
  stat_slab(normalize = "panels", alpha = 0.3) +
  facet_grid(~ spp, labeller = label_both) +
  # labs(x = "Height (m)", y = "Treatment") +
  scale_color_manual(
    values = rnd_color_brewer("Set2", c(1,4)),
    aesthetics = c("color", "fill")
  ) +
  scale_x_continuous(limits = c(-2, 16))

################################################################################
################################# A macro plot #################################
################################################################################
library(ggforce)
library(ggplot2)
library(dplyr)

max <- 44.7
# CW from bottom left
xdir <- c(1, 1, -1, -1)
ydir <- c(1, -1, -1, 1)
corner_inset <- 10 / sqrt(2)
centerx <- xdir * corner_inset + c(0, 0, max, max)
centery <- ydir * corner_inset + c(0, max, max, 0)
centers <- data.frame(x = centerx, y = centery)
regen <- cbind(centers, r = 4)
transects <- rbind(centers, centers)
transect_endx <- xdir * 10 + centerx
transect_endy <- ydir * 10 + centery
# Half of the x and y coordinates of the transect ends are the same as the
# center, and half are different.
transects$xend <- c(transect_endx, centerx)
transects$yend <- c(centery, transect_endy)
# distance of samp. cyls. from regen centers 
x = c(1, 1, 0, 0, 0, 0, 1, 1, -1, -1, 0, 0, 0, 0, -1, -1)
y = c(0, 0, 1, 1, -1, -1, 0, 0, 0, 0, -1, -1, 1, 1, 0, 0)
samp_cyl <- data.frame(
  x = x * c(9, 5) + rep(centerx, each = 4),
  y = y * c(9, 5) + rep(centery, each = 4),
  r = 1
)

plot_design <- function() {
  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = max, ymax = max),
    color = "black",
    fill = NA
  ) +
  # geom_point(aes(centerx, centery)) +
  geom_circle(data = regen, aes(x0 = x, y0 = y, r = r), color = "#008000") +
  geom_circle(data = samp_cyl, aes(x0 = x, y0 = y, r = r), color = "#ff7f00") +
  geom_segment(data = transects, aes(x, y, xend = xend, yend = yend)) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0, "in"),
    aspect.ratio = 1,
    plot.background = element_blank()
  ) +
  scale_x_continuous(position = "top") +
  labs(x = "Treatment", y = "Site")
}

experiment_design <- function() {
  treatment <- c("GS", "LD", "HA", "HD")
  site <- factor(c(1, 2, 3, 4))
  all_transects <- tidyr::expand_grid(treatment, site, transects)
  all_regen <- tidyr::expand_grid(treatment, site, regen)
  all_samp_cyl <- tidyr::expand_grid(treatment, site, samp_cyl)

  ggplot() +
  geom_rect(
    aes(xmin = 0, ymin = 0, xmax = max, ymax = max),
    color = "black",
    fill = NA
  ) +
  # geom_point(aes(centerx, centery)) +
  geom_circle(data = all_regen, aes(x0 = x, y0 = y, r = r), color = "#008000") +
  geom_circle(data = all_samp_cyl, aes(x0 = x, y0 = y, r = r), color = "#ff7f00") +
  geom_segment(data = all_transects, aes(x, y, xend = xend, yend = yend)) +
  facet_grid(
    site ~ treatment,
    switch = "y"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0, "in"),
    aspect.ratio = 1,
    plot.background = element_blank()
  ) +
  scale_x_continuous(position = "top") +
  labs(x = "Treatment", y = "Site")
}


design <- "
############
############
############
############
############
1###########
"

source("./scripts/design.r")

plot_design("transect") + ggplot2::expand_limits(x = 300, y = 200)
  ggplot2::ggsave("figures/transect_bg.png")
plot_design("macro") + ggplot2::expand_limits(x = 300, y = 200)
  ggplot2::ggsave("figures/macro_bg.png")
plot_design("regen") + ggplot2::expand_limits(x = 300, y = 200)
  ggplot2::ggsave("figures/regen_bg.png")
plot_design("samp_cyl") + ggplot2::expand_limits(x = 300, y = 200)
  ggplot2::ggsave("figures/samp_cyl_bg.png")


wrangle_datasheet("../data/fuel_camp6_gs.csv")




library(vegan)
data(dune)
data(dune.env)
dune
dune.env
## default test by terms
adonis2(dune ~ Management*A1, data = dune.env)
## overall tests
adonis2(dune ~ Management*A1, data = dune.env, by = NULL)

### Example of use with strata, for nested (e.g., block) designs.
(dat <- expand.grid(rep=gl(2,1), NO3=factor(c(0,10)),field=gl(3,1, labels = letters[1:3])))
(Agropyron <- with(dat, as.numeric(field) + as.numeric(NO3)+2) +rnorm(12)/2)
(Schizachyrium <- with(dat, as.numeric(field) - as.numeric(NO3)+2) +rnorm(12)/2)
(total <- Agropyron + Schizachyrium)

dotplot(total ~ NO3, dat, jitter.x=TRUE, groups=field,
        type=c('p','a'), xlab="NO3", auto.key=list(columns=3, lines=TRUE) )

(Y <- data.frame(Agropyron, Schizachyrium))
(mod <- metaMDS(Y, trace = FALSE))
plot(mod)

### Ellipsoid hulls show treatment
with(dat, ordiellipse(mod, field, kind = "ehull", label = TRUE))
### Spider shows fields
with(dat, ordispider(mod, field, lty=3, col="red"))

### Incorrect (no strata)
adonis2(Y ~ NO3, data = dat, permutations = 199)
## Correct with strata
with(dat, adonis2(Y ~ NO3, data = dat, permutations = 199, strata = field))

