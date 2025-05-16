
hundhr_post_data <- dpost$data[[3]]
hundhr_post_data <- hundhr_post_data |> 
  mutate(
    block = paste(site, treatment),
    corner = paste(block, corner)
  )

form1 <- load ~ treatment + (1 | site) + (1 | block) + (1 | corner)
hundhr_post1
hundhr_post1 <- glmmTMB(
  formula = form1,
  data = hundhr_post_data,
  family = ziGamma(link = "log"),
  ziformula = ~ treatment + site,
  dispformula = ~ treatment
)
hundhr_post2 <- glmmTMB(
  formula = form1,
  data = hundhr_post_data,
  family = ziGamma(link = "log"),
  ziformula = ~ treatment,
  dispformula = ~ treatment
)

formula <- load ~ treatment + (1 | site) + (1 | block) + (1 | corner)
fuel_tmb$pre2 <- run_glmmtmb_mod(dpre, mods)

fuel_tmb$post$aic
fuel_tmb$post2$aic


coef(fuel_tmb$post$mod[[1]])[[1]][["site"]]
coef(fuel_tmb$post2$mod[[1]])[[1]][["site"]]
fixef(fuel_tmb$post2$mod[[1]])
fixef(fuel_tmb$post$mod[[1]])

par(mfrow = c(3, 2))
fuel_tmb$post2 |> group_walk(\(x, ...){
  plotResiduals(x$res[[1]], form = x$data[[1]]$treatment)
  title(sub = x$class)
})

par(mfrow = c(3, 2))
fuel_tmb$post |> group_walk(\(x, ...){
  plotResiduals(x$res[[1]], form = x$data[[1]]$treatment)
  title(sub = x$class)
})

plotResiduals(fuel_tmb$post$res[[3]])
plotResiduals(fuel_tmb$post2$res[[3]])

simulateResiduals(hundhr_post1) |>
  plotResiduals()

AIC(hundhr_post1, hundhr_post2)


predictions(hundhr_post1, by = "treatment")
predictions(hundhr_post2, by = "treatment")
