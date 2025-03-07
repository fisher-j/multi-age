
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

p1 + p2
plot(ht10emm, comparisons = TRUE)
emmcomp <- plot(ht10emm, comparisons = TRUE, plotit = FALSE) |> as_tibble()

dht10 |>
  ggplot(aes(treat, ht, color = spp, fill = spp)) +
  facet_grid(~spp, switch = "x") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        ) +
  geom_dots() +
  scale_color_brewer(palette = "Set2", aesthetics = c("color", "fill")) +
  geom_pointrange(
    data = emmcomp,
    aes(y = the.emmean, ymin = lower.CL, ymax = upper.CL),
    color = "gray60",
    position = position_nudge(x = -0.07),
    size = 0.7,
    linewidth = 2.3,
    show.legend = FALSE
  ) +
  geom_segment(
    data = emmcomp,
    aes(y = the.emmean, xend = treat, yend = rcmpl),
    color = "dodgerblue4",
    lineend = "butt",
    linejoin = "bevel",
    arrow = arrow(length = grid::unit(2.5, "mm")),
    position = position_nudge(x = -0.07),
    linewidth = 1
  ) +
  geom_segment(
    data = emmcomp,
    aes(y = the.emmean, xend = treat, yend = lcmpl),
    color = "dodgerblue4",
    lineend = "butt",
    linejoin = "bevel",
    arrow = arrow(length = grid::unit(2.5, "mm")),
    position = position_nudge(x = -0.07),
    linewidth = 1
  )

scales::show_col(colors()[!grepl("gr[ea]y", colors())], cex_label = 0.5)


emmeans( mht18, spec = "treat", by = c("spp"), 
  at = list(year = "10"), type = "response") |> pairs()

##### how many little tanoak sprouts? #####
regen
regen |> filter(dbh < 2.54) |>
  group_by(treat) |>
  summarize(ba_ha = round(sum(per_ha["treat"] * for_const(dbh)), 2)) |>
  arrange(ba_ha)


d$transects |>
  select(all_of(transectid), slope, matches(veg_match)) |>
  pivot_longer(
    !c(any_of(transectid), slope),
    names_to = c(".value", "station"),
    names_pattern = "(\\w+)([12])"
  ) |> 
  # there was some inconsistency in whether heights were zero or blank if no veg
  # was present, here I sort that out.
  # We are interested in total load, but the dead component has implications for
  # fuel moisture. We recorded percent cover of live and dead separately, so
  # here i calculate a total and proportion dead.
  mutate(
    woody_ht = if_else(live_woody == 0 & dead_woody == 0, 0, avg_w_ht),
    herb_ht = if_else(live_herb == 0 & dead_herb == 0, 0, avg_h_ht),
    woody_load = ((live_woody + dead_woody) / 100) * woody_ht * 18,
    woody_p_dead = if_else(
      woody_load == 0, 0, dead_woody / (live_woody + dead_woody)
    ),
    herb_load = ((live_herb + dead_herb) / 100) * herb_ht * 8,
    herb_p_dead = if_else(
      herb_load == 0, 0, dead_herb / (live_herb + dead_herb)
    )
  ) |>  select(-c(phase, site, treatment, corner, slope))
  select(!matches("live_|dead_|avg_"), woody_ht, herb_ht) |>
  pivot_longer(
    matches("woody|herb"),
    names_to = c("class", ".value"),
    names_pattern = "(woody|herb)_(.*)"
  )

