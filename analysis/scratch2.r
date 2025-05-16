
##### trying to figure out how emmeans works with zero inflated models #####

fuel_tmb$post$mod[[4]] |> 
  comparisons(variables = list(treatment = "pairwise"))

predict(fuel_tmb$post$mod[[4]], type = "response")

fuel_tmb$post$mod[[2]] |> insight::get_data()
fuel_tmb$pre$mod[[1]] |> comparisons(newdata = print(datagrid()))
fuel_tmb$post
emmeans::emmeans(hundhr, "treatment", component = "response", re.form = NA)
emmeans::emmeans(hundhr, "treatment", type = "response")

onehr_data <- unnest(filter(dpre, class == "onehr"), data)
onehr <- glmmTMB(
  form,
  data = onehr_data,
  family = ziGamma(link = "log"),
  ziformula = ~1
)

emmeans::emmeans(onehr, "treatment", component = "response")
emmeans::emmeans(onehr, "treatment", type = "response")

library(marginaleffects)
# install.packages("modelbased")

# I just realized that these two calls are the same, so I have a path forward
# for calculating accurate predictions
emmeans::emmeans(onehr, "treatment", component = "response")
predictions(
  onehr,
  by = "treatment",
  re.form = NA,
)

emmeans::emmeans(onehr, "treatment", component = "response") |> pairs()
avg_comparisons(
  onehr,
  variables = list(treatment = "pairwise"),
  re.form = NA,
) |>
  as.data.frame() |>
  mutate(p.adjust1 = p.adjust(p.value)) |>
  select(contrast, estimate, p.value, p.adjust1, p.adjust2)


predictions(
  mba,
  re.form = NA
) |>
  as.data.frame() |>
  mutate(p.adjust = p.adjust(p.value))



avg_comparisons(
  onehr,
  variables = list(treatment = "pairwise"),
  re.form = NA,
) |>
  as.data.frame() |>
  mutate(p.adjust = p.adjust(p.value))


datagrid(treatment = unique, model = onehr)

modelbased::estimate_contrasts(onehr, "treatment", re.form = NA) |> plot()
out <- modelbased::estimate_means(onehr, re.form = NA)

predictions(
  onehr,
  by = "treatment",
  hypothesis = difference ~ pairwise,
  re.form = NA
)

dput(onehr_data)
onehr_data

fuel_tmb$pre$emmeans[[1]] |>
  ggplot(aes(x = treatment, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_linerange()

emm_mapping()

comparisons(
  mba,
  newdata = datagrid(treat = unique, spp = unique, site = NA),
  variables = list(treat = "pairwise"),
  by = "spp",
  re.form = NA,
) |> 
  as.data.frame() |>
  # group_by(spp) |>
  mutate(p.adjust = p.adjust(p.value))

predictions(
  mba,
  newdata = "balanced"
  # by = "treat",
  # byfun = sum
)

datagrid(treat = unique, spp = unique, site = NA, model = mba)
 
regen_pa$site |> unique()
regen_po

avg_predictions(
  mba,
  newdata = datagrid(treat = unique, spp = unique, site = c(NA)),
  by = "treat",
  byfun = sum,
  re.form = NA
) |> as.data.frame() |>
  select(-c(s.value)) |>
  tinytable::tt(digits = 2)

# Compare sum of basal area across treatments
avg_predictions(
  mba,
  newdata = datagrid(treat = unique, spp = unique, site = c(NA)),
  by = "treat",
  byfun = sum,
  re.form = NA,
  hypothesis = difference ~ pairwise
) |> 
  as.data.frame() |>
  mutate(p.adjust = p.adjust(p.value)) |>
  select(-c(s.value), contrast = hypothesis) |>
  tinytable::tt(digits = 2)

avg_predictions(
  mba, 
  newdata = datagrid(treat = unique, spp = unique, site = c(NA)),
  by = c("treat", "spp"),
  re.form = NA
)

datagrid(model = mba, treat = unique, spp = unique, site = c("waldon", "camp6"))

as_tibble() |>
  mutate(p.value = p.adjust(p.value))

regen_pa |> 
  group_by(treat, site, plot) |>
  summarize(ba_ha = sum(ba_ha)) |>
  summarize(ba_ha = mean(ba_ha)) |>
  summarize(ba_ha = mean(ba_ha))


    # emmeans = list(
    #   predictions(mod, by = "treatment", re.form = NA)
    # ),
    # pairs = list(
    #   avg_comparisons(
    #     mod,
    #     variables = list(treatment = "pairwise"),
    #     re.form = NA,
    #   )
    # ),

fuel_tmb$pre |>
  mutate(class = fuel_class_labels[as.character(class)]) |>
  group_map(function(x, ...) {
    x$mod[[1]] |>
      predictions(
        # newdata = datagrid()
        by = "treatment",
        re.form = NA
      ) |>
      ggplot(aes(x = treatment, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_point() +
      geom_linerange() +
      labs(title = x$class, y = expression(Load~(Mg%.%ha^-1)), x = "Treatment")
  }) |>
  patchwork::wrap_plots() + patchwork::plot_layout(axes = "collect")

fuel_tmb$pre |>
  mutate(class = fuel_class_labels[as.character(class)]) |>
  group_map(function(x, ...) {
    x$mod[[1]] |>
      predictions(
        type = "response",
        newdata = datagrid(site = NA, corner = NA, treatment = unique),
        by = "treatment",
        re.form = NA
      ) |>
      ggplot(aes(x = treatment, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_point() +
      geom_linerange() +
      labs(title = x$class, y = expression(Load~(Mg%.%ha^-1)), x = "Treatment")
  }) |>
  patchwork::wrap_plots() + patchwork::plot_layout(axes = "collect")

dpre$data[[1]]

################################################################
###### Begin developing function to get comparison arrows ######
################################################################

# This is the comparisons function call that I wan to use
# the by argument is "contrast"
# start with predictions
pred <- fuel_tmb$post$mod[[3]] |> predictions(by = "treatment", re.form = NA)
comp <- fuel_tmb$post$mod[[3]] |> 
  comparisons(
    # newdata = datagrid(site = NA, corner = NA, treatment = unique),
    variables = list(treatment = "pairwise"),
    by = "contrast",
    re.form = NA
  )
pred
comp

# # the idea here was to generate the comparisons directly from the predictions so
# # I would have a predictable result.
# hyp <- hypotheses(pred, difference ~ pairwise)
# b_vars_names <- function(b_vars, pred_object, focal_var) {
#   stringr::str_replace_all(
#     b_vars,
#     setNames(
#       as.character(pred[[focal_var]]), # assume first column is focal var
#       paste0("b", seq_len(nrow(pred)))
#     )
#   ) |>
#     str_replace_all("\\(|\\)", "")
# }
# hyp <- mutate(hyp, hypothesis = b_vars_names(hypothesis, pred, "treatment")) |>
#   tidyr::separate_wider_delim(
#     hypothesis,
#     " - ",
#     names = c("var1", "var2"),
#     cols_remove = FALSE
#   )
#

emmeans_obj <- emmeans(fuel_tmb$pre$mod[[1]], "treatment", type = "response")
psumm <- confint(pairs(emmeans_obj), type = "lp")

extra <- emmeans_obj
(summ <- summary(extra))
(est <- summ[["response"]])
(psumm <- confint(pairs(extra), type = "lp")) #pairwise summary
( del <- (psumm[[6]] - psumm[[5]]) / 4 ) # half half-width of confidnce interval
( diff = psumm[[2]] ) # estimate of pairwise differences
( overlap = apply(psumm[ ,5:6], 1, function(x) 2*min(-x[1],x[2])/(x[2]-x[1])) )
pbv <- rep(1, nrow(psumm)) # we don't use "by" variables
lbv <- rep(1, nrow(summ)) # we don't use "by" variables
ubv <- 1 # we don't use "by" variables
neach <- length(lbv) / length(ubv)
# establish the order of variables involved in comparison
id1 = rep(seq_len(neach-1), rev(seq_len(neach-1)))
id2 = unlist(sapply(seq_len(neach-1), function(x) x + seq_len(neach-x)))
# list of psumm row numbers involved in each summ row
involved = lapply(seq_len(neach), function(x) union(which(id2==x), which(id1==x)))

# initialize arrays
mind <- numeric(length(lbv)) # min difference?
llen <- rlen <- numeric(neach) # right and left lengths
npairs <- length(id1)
iden <- diag(rep(1, 2 * neach))

# ommiting outer for loop for groups, we only are using 1 group
by <- 1 # we are only using a single group
d <- del[pbv == by] # we only have 1 group, no by var
rows <- which(lbv == by) # we only have 1 group, no by var
for(i in seq_len(neach)) {
  mind[rows[i]] = min(d[involved[[i]]])
} 

# Set up regression equations to match arrow overlaps with interval overlaps
# We'll add rows later (with weights 1) to match with mind values
lmat <- rmat <- matrix(0, nrow = npairs, ncol = neach)
y <- numeric(npairs)
v1 <- 1 - overlap[pbv == by] # 1 because 1 group
dif = diff[pbv == by] # same same
for (i in which(!is.na(v1))) {
  #wgt = 6 * max(0, ifelse(v1[i] < 1, v1[i], 2-v1[i]))
  wgt = 3 + 20 * max(0, .5 - (1 - v1[i])^2)
  # really this is sqrt of weight
  if (dif[i] > 0)   # id2  <----->  id1
  lmat[i, id1[i]] = rmat[i, id2[i]] = wgt*v1[i]
  else  # id1  <----->  id2
  rmat[i, id1[i]] = lmat[i, id2[i]] = wgt*v1[i]
  y[i] = wgt * abs(dif[i])
}

X = rbind(cbind(lmat, rmat),iden)
y = c(y, rep(mind[rows], 2))
y[is.na(y)] = 0
soln = qr.coef(qr(X), y)
soln[is.na(soln)] = 0
ll = llen[rows] = soln[seq_len(neach)]
rl = rlen[rows] = soln[neach + seq_len(neach)]

# Abort if negative lengths
if (any(c(rl, ll) < 0)) {
  stop("Aborted -- Some comparison arrows have negative length!\n",
    "(in group \"", by, "\")",
    call. = FALSE)
}

# Overlap check
for (i in which(!is.na(v1))) {
  v = 1 - v1[i]
  obsv = 1 - abs(dif[i]) / ifelse(dif[i] > 0, ll[id1[i]] + rl[id2[i]], rl[id1[i]] + ll[id2[i]])
  if (v*obsv < 0) {
    warning("Comparison discrepancy in group \"", by, 
      "\", ", psumm[i, 1], 
      ":\n    Target overlap = ", round(v, 4),
      ", overlap on graph = ", round(obsv, 4),
      call. = FALSE)
  }
}

# shorten arrows that go past the data range
estby = est[rows]
rng = suppressWarnings(range(estby, na.rm = TRUE))
diffr = diff(rng)
ii = which(estby - ll < rng[1])
llen[rows][ii] = estby[ii] - rng[1] + .02 * diffr
ii = which(estby + rl > rng[2])
rlen[rows][ii] = rng[2] - estby[ii] + .02 * diffr

# remove arrows completely from extremes
llen[rows][estby < rng[1] + .0001 * diffr] = NA
rlen[rows][estby > rng[2] - .0001 * diffr] = NA

list(
  lcmpl = as.numeric(est - llen),
  rcmpl = as.numeric(est + rlen)
)

predictions_est <- summary(emmeans_obj)[[2]]
predictions_var <- summary(emmeans_obj)[[1]]
pair <- confint(pairs(emmeans_obj), type = "lp") |>
  as_tibble() |>
  tidyr::separate_wider_delim(
    contrast,
    " - ",
    names = c("var1", "var2"),
    cols_remove = FALSE
  ) |>
  select(var1, var2, est = estimate, conf.low = asymp.LCL, conf.high = asymp.UCL)
pred <- data.frame(
  est = predictions_est,
  var = predictions_var
)

get_comparisons_emmeans(pred, pair)

############### THis is my function to get comparisons #########################

get_comparisons_emmeans <- function(estby, pair) {
  # half half-width of confidence interval
  del <- (pair[["conf.high"]] - pair[["conf.low"]]) / 4
  diff <- pair[["est"]] # estimate of pairwise differences
  overlap <- apply(
    pair[c("conf.low", "conf.high")],
    1,
    function(x) 2 * min(-x[1], x[2]) / (x[2] - x[1])
  )
  neach <- nrow(estby)
  id1 <- match(pair$var1, estby$var)
  id2 <- match(pair$var2, estby$var)
  involved <- lapply(
    seq_len(neach),
    function(x) union(which(id2 == x), which(id1 == x))
  )
  mind <- numeric(nrow(estby)) # min difference?
  llen <- rlen <- numeric(nrow(estby)) # right and left lengths
  npairs <- nrow(pair)
  iden <- diag(rep(1, 2 * neach))

  d <- del # we only have 1 group, no by var
  rows <- seq_len(nrow(estby)) # we only have 1 group, no by var
  for (i in seq_len(neach)) {
    mind[rows[i]] <- min(d[involved[[i]]])
  }

  # Set up regression equations to match arrow overlaps with interval overlaps
  # We'll add rows later (with weights 1) to match with mind values
  lmat <- rmat <- matrix(0, nrow = npairs, ncol = neach)
  y <- numeric(npairs)
  v1 <- 1 - overlap
  dif <- diff

  for (i in which(!is.na(v1))) {
    #wgt = 6 * max(0, ifelse(v1[i] < 1, v1[i], 2-v1[i]))
    wgt = 3 + 20 * max(0, .5 - (1 - v1[i])^2)
    # really this is sqrt of weight
    if (dif[i] > 0) {
      # id2  <----->  id1
      lmat[i, id1[i]] <- rmat[i, id2[i]] <- wgt * v1[i]
    } else { 
      # id1  <----->  id2
      rmat[i, id1[i]] <- lmat[i, id2[i]] <- wgt * v1[i]
    }
    y[i] <- wgt * abs(dif[i])
  }

  X <- rbind(cbind(lmat, rmat), iden)
  y <- c(y, rep(mind[rows], 2))
  y[is.na(y)] <- 0
  soln <- qr.coef(qr(X), y)
  soln[is.na(soln)] <- 0
  ll <- llen[rows] <- soln[seq_len(neach)]
  rl <- rlen[rows] <- soln[neach + seq_len(neach)]

  # Abort if negative lengths
  if (any(c(rl, ll) < 0)) {
      stop("Aborted -- Some comparison arrows have negative length!\n",
           "(in group \"", by, "\")",
           call. = FALSE)
  }

  # Overlap check
  for (i in which(!is.na(v1))) {
    v <- 1 - v1[i]
    obsv <- 1 - abs( dif[i]) / ifelse(dif[i] > 0, ll[id1[i]] + rl[id2[i]], rl[id1[i]] + ll[id2[i]])
    if (v*obsv < 0) {
      warning("Comparison discrepancy in group \"", by, 
              "\", ", psumm[i, 1], 
              ":\n    Target overlap = ", round(v, 4),
              ", overlap on graph = ", round(obsv, 4),
              call. = FALSE)
    }
  }

  # shorten arrows that go past the data range
  
  estby <- pred$est
  rng <- suppressWarnings(range(estby, na.rm = TRUE))
  diffr <- diff(rng)
  ii <- which(estby - ll < rng[1])
  llen[rows][ii] <- estby[ii] - rng[1] + .02 * diffr
  ii <- which(estby + rl > rng[2])
  rlen[rows][ii] <- rng[2] - estby[ii] + .02 * diffr

  # remove arrows completely from extremes
  llen[rows][estby < rng[1] + .0001 * diffr] = NA
  rlen[rows][estby > rng[2] - .0001 * diffr] = NA

  list(
    lcmpl = as.numeric(pred$est - llen),
    rcmpl = as.numeric(pred$est + rlen)
  )
}

data(sleepstudy,package="lme4")
g0 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
predict(g0, newdata = data.frame(Days = 1:9, Subject = NA), re.form = NA)


library("emmeans")
library("glmmTMB")
data(Salamanders)
zipm3 = glmmTMB(count~spp * mined + (1|site), zi=~spp * mined, Salamanders, family="poisson")
emmeans(zipm3, ~mined, component = "response", re.form = NA)

zipm4 = glmmTMB(count~spp * mined + (1|site), zi=~site, Salamanders, family="poisson")
emmeans(zipm4, ~mined, component = "response", re.form = NA)


library("marginaleffects")
library("glmmTMB")
data(Salamanders)
zipm3 = glmmTMB(
  count~spp * mined + (1|site:spp),
  zi=~spp * mined,
  Salamanders, 
  family="poisson"
)

avg_comparisons(zipm3, variables = list(mined = "pairwise"))
insight::get_data(zipm3) |> head()

library("marginaleffects")
library("glmmTMB")
data(Salamanders)
zipm4 = glmmTMB(
  count~spp * mined + (1|site:spp:mined),
  zi=~spp * mined,
  Salamanders,
  family="poisson"
)
comparisons(zipm4, variables = list(mined = "pairwise"), by = "contrast")
predictions(zipm4, by = "mined", type = "response")


