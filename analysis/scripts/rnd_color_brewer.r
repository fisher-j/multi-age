
# Quickly iterate over random color selections and pick the colors I like
rnd_color_brewer <- function(palette, chosen_order = NULL) {
  n <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
  ord <- sample(1:n)
  if (!is.null(chosen_order)) ord <- chosen_order
  colors <- RColorBrewer::brewer.pal(n, palette)[ord]
  print(ord)
  colors
}
