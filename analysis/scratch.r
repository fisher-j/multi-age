
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

