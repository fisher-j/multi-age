# RD02: retain the thesis basal-area model; change the prediction target.
# From publication/: Rscript scripts/regeneration/01-generate-rd02-display.R
# May also be sourced. All outputs are publication-only; no model is refitted.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else sys.frame(1)$ofile
script_dir <- dirname(normalizePath(script_file))
publication_root <- normalizePath(file.path(script_dir, "..", ".."))
source(file.path(publication_root, "scripts", "publication-paths.R"))
paths_common <- publication_paths(publication_root)
suppressPackageStartupMessages({
  library(glmmTMB)
  library(dplyr)
  library(ggplot2)
  library(marginaleffects)
})
input <- file.path(paths_common$input, "regeneration-measurements.rds")
input_hash <- tools::md5sum(input)
saved <- list(regen_pa = readRDS(input))
out <- file.path(paths_common$output, "regeneration")
for (directory in c("figures", "tables", "inference-audit", "manifests"))
  dir.create(file.path(out, directory), recursive = TRUE, showWarnings = FALSE)
reference_root <- file.path(paths_common$input, "regeneration")
for (directory in c("tables", "inference-audit")) {
  files <- list.files(file.path(reference_root, directory), full.names = TRUE)
  stopifnot(all(file.copy(files, file.path(out, directory), overwrite = TRUE)))
}
estimates <- read.csv(file.path(out, "tables/rd02-basal-area-model-estimates.csv"))
estimates$treat <- factor(estimates$treat, levels = c("gs", "ld", "ha", "hd"))
estimates$spp <- factor(estimates$spp, levels = c("rw", "to", "df", "other"))
variance <- unique(estimates$random_intercept_variance)
multiplier <- unique(estimates$integration_multiplier)
display <- filter(estimates, spp %in% c("rw", "to"))
pos <- position_dodge(width = .44)
figure <- ggplot(display, aes(treat, estimate, ymin = lcl, ymax = ucl,
                             color = spp, group = spp)) +
  geom_linerange(linewidth = 1.6, position = pos) +
  geom_point(aes(shape = spp), fill = "black", color = "white", stroke = .6,
             size = 2.8, position = pos) +
  scale_color_manual(values = c(rw = "#3F51B5", to = "#C96F38"),
                     labels = c(rw = "Redwood", to = "Tanoak")) +
  scale_shape_manual(values = c(rw = 21, to = 24),
                     labels = c(rw = "Redwood", to = "Tanoak")) +
  scale_x_discrete(labels = c(gs = "GS", ld = "LD", ha = "HA", hd = "HD")) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 30, 5),
                     expand = expansion(mult = c(0, .06))) +
  labs(x = "Retention treatment", y = expression(Basal~area~(m^2 %.% ha^-1)),
       color = NULL, shape = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), axis.title.y = element_text(margin = margin(r = 10)))
figure_path <- file.path(out, "figures/rd02-regeneration-basal-area.png")
ggsave(figure_path, figure, width = 7, height = 4.4, dpi = 320, bg = "white")
ggsave(sub("png$", "pdf", figure_path), figure, width = 7, height = 4.4)
asset <- file.path(paths_common$assets, "", basename(figure_path))
stopifnot(file.copy(figure_path, asset, overwrite = TRUE),
          identical(unname(tools::md5sum(input)), unname(input_hash)))

provenance <- list(
  task = "RD02", status = "Draft pending scientific and display review",
  script = "publication/scripts/regeneration/01-generate-rd02-display.R",
  script_md5 = unname(tools::md5sum(script_file)),
  source = "publication/inputs/thesis/regeneration-measurements.rds", source_md5 = unname(input_hash),
  source_object = "exported historical mba reference", model_refitted = FALSE,
  historical_reference = "publication/inputs/thesis/regeneration",
  historical_replay = "analysis/scripts/export-publication-inputs.R using thesis R and renv",
  family = "zero-inflated Gamma (log conditional link; logit zero link)",
  n_species_plot_rows = nrow(saved$regen_pa), n_vegetation_plots = 64L,
  n_macro_plots = 16L, n_sites = 4L,
  random_intercept_variance = variance, integration_multiplier = multiplier,
  site_weights = "equal; no site fixed effects, so integrated expectations coincide across site rows",
  interval = "95% log-scale delta method; full nonrandom parameter covariance including log SD",
  checks = "Historical 16-cell prediction, gradient, and quadrature checks executed by thesis exporter; publication validates export checksums",
  review = "Retained thesis site:species intercept does not explicitly model macro-plot clustering; manuscript design review remains open. Intervals condition on this model."
)
yaml::write_yaml(provenance, file.path(out, "manifests/rd02-figure-provenance.yml"))
capture.output(sessionInfo(), file = file.path(out, "manifests/rd02-session-info.txt"))
# Bundle linked review downloads inside the Quarto project so they also work
# from docs/publication/ and on the published website.
download_dir <- file.path(paths_common$assets, "rd02")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
downloads <- c(
  list.files(file.path(out, "tables"), full.names = TRUE),
  list.files(file.path(out, "inference-audit"), full.names = TRUE),
  list.files(file.path(out, "manifests"), full.names = TRUE),
  sub("png$", "pdf", figure_path), script_file, file.path(script_dir, "README.md")
)
stopifnot(all(file.copy(downloads, download_dir, overwrite = TRUE)))
message("RD02: verified 16 species/treatment cells; generated eight-cell lead figure. Multiplier = ",
        signif(multiplier, 6))
print(as.data.frame(display[, c("spp", "treat", "estimate", "lcl", "ucl")]))
