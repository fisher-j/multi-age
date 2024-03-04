################################################################################
################################# A macro plot #################################
################################################################################

make_data <- function() {
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
  treatment <- c("GS", "LD", "HA", "HD")
  site <- factor(c(1, 2, 3, 4))
  col <- c(macro = "black", regen = "#008000", samp_cyl = "#ff7f00", transect = "#ff7f00")
  list(
    site = site, treatment = treatment, col = col, max = max, regen = regen,
    transects = transects, samp_cyl = samp_cyl
  )
}

plot_design <- function(type = c("macro", "transect", "regen", "samp_cyl")) {
  d <- make_data()
  col <- dplyr::if_else(names(d$col) %in% type, d$col, "gray80")
  names(col) <- names(d$col)
  ggplot2::ggplot() +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = 0, ymin = 0, xmax = d$max, ymax = d$max),
    color = col["macro"],
    fill = NA,
    linewidth = 1.2,
  ) +
  ggforce::geom_circle(
    data = d$regen,
    ggplot2::aes(x0 = x, y0 = y, r = r),
    color = col["regen"],
    linewidth = 1.2
  ) +
  ggforce::geom_circle(
    data = d$samp_cyl,
    ggplot2::aes(x0 = x, y0 = y, r = r),
    color = col["samp_cyl"],
    linewidth = 1.2
  ) +
  ggplot2::geom_segment(
    data = d$transects,
    ggplot2::aes(x, y, xend = xend, yend = yend),
    color = col["transect"],
    linewidth = 1.2
  ) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(0, "in"),
    plot.background = ggplot2::element_blank()
  ) +
  ggplot2::scale_x_continuous(position = "top") +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::coord_fixed(ratio = 1)
}

experiment_design <- function(type = c("macro", "transect", "regen", "samp_cyl")) {
  treatment <- forcats::fct_inorder(c("GS", "LD", "HA", "HD"))
  site <- factor(c(1, 2, 3, 4))
  d <- make_data()
  col <- dplyr::if_else(names(d$col) %in% type, d$col, "gray80")
  names(col) <- names(d$col)
  ggplot2::ggplot() +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = 0, ymin = 0, xmax = d$max, ymax = d$max),
    color = col["macro"],
    fill = NA,
    linewidth = 1.2
  ) +
  ggforce::geom_circle(
    data = tidyr::expand_grid(treatment, site, d$regen),
    ggplot2::aes(x0 = x, y0 = y, r = r),
    color = col["regen"],
    linewidth = 1.2
  ) +
  ggforce::geom_circle(
    data = tidyr::expand_grid(treatment, site, d$samp_cyl),
    ggplot2::aes(x0 = x, y0 = y, r = r),
    color = col["samp_cyl"],
    linewidth = 1.2
  ) +
  ggplot2::geom_segment(
    data = tidyr::expand_grid(treatment, site, d$transects),
    ggplot2::aes(x, y, xend = xend, yend = yend),
    linewidth = 1.2,
    color = col["transect"]
  ) +
  ggplot2::facet_grid(
    site ~ treatment,
    switch = "y"
  ) +
  ggplot2::theme_gray(base_size = 20) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(0, "in"),
    plot.background = ggplot2::element_blank()
  ) +
  ggplot2::scale_x_continuous(position = "top") +
  ggplot2::labs(x = "Treatment", y = "Site") +
  ggplot2::coord_fixed(ratio = 1)
}

