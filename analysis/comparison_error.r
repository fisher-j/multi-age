library("marginaleffects")
library("glmmTMB")
data(Salamanders)

zipm3 = glmmTMB(
  count~spp * mined + (1|site),
  Salamanders, 
  family="poisson"
)
avg_comparisons(zipm3, variables = list(mined = "pairwise"))

zipm5 = glmmTMB(
  count~spp * mined + (1|mined),
  Salamanders, 
  family="poisson"
)
avg_comparisons(zipm5, variables = list(mined = "pairwise"))

zipm5 = glmmTMB(
  count~spp * mined + (1|site:mined),
  Salamanders, 
  family="poisson"
)
avg_comparisons(zipm5, variables = list(mined = "pairwise"))

predict(zipm5, newdata = head(Salamanders, 23))

zipm5 = glmmTMB(
  count~spp * mined + (1|spp:mined),
  Salamanders, 
  family="poisson"
)
avg_comparisons(zipm5, variables = list(mined = "pairwise"))


zipm4 = glmmTMB(
  count~spp * mined + (1|spp:site),
  Salamanders,
  family="poisson"
)
comparisons(zipm4, variables = list(mined = "pairwise"))

zipm4 = glmmTMB(
  count~spp * mined + (1|spp),
  Salamanders,
  family="poisson"
)
comparisons(zipm4, variables = list(mined = "pairwise"))

