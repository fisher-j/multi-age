# This function processess one datasheet and returns three tables that can
# be combined with corresponding tables from other data sheets
wrangle_datasheet <- function(file) {
  con <- file(file, encoding = "UTF-8")
  lines <- readLines(con)
  close(con)

  # Remove BOM <https://stackoverflow.com/a/67906611>
  lines[1] <- gsub("\\xef\\xbb\\xbf", "", lines[1], useBytes = TRUE)

  # These are the sections I want to extract
  to_get <- c(
    "site_data", "transects", "duff_litter_fbd", "vegetation",
    "woody_species", "coarse_woody_debris"
  )
  # sections are defined by line with only a hashtage (#section)
  section_pattern <- "^#(\\w+).*$"

  # find hashtags and get the data from the next line to the line
  # before the next hashtag
  section_breaks <- grep(section_pattern, lines)
  section_start <- section_breaks + 1
  section_end <- c(section_breaks[-1], length(lines)) - 1
  section_names <- gsub(section_pattern, "\\1", lines[section_breaks])

  sections <- purrr::map2(section_start, section_end, \(x, y) c(x, y)) |>
    setNames(section_names) |>
    (`[`)(to_get) |>
    purrr::map(\(x) lines[seq.int(x[1], x[2])]) |>
    # collapse sections to strings so they can be read as if they were files
    purrr::map(\(x) paste(x, collapse = "\n")) |>
    # leave empty column names so they can be removed
    purrr::map(\(x) 
      readr::read_csv(x, show_col_types = FALSE, name_repair = "minimal")
    ) |>
    # Remove empty columns
    purrr::map(\(x) x[!names(x) %in% ""])

  # I'm going to combine these into a wide table because each row is a transect.
  # Further data wrangling will require expanding the stations within transecs.
  # also need to make sure the rows have site data and transect ids for the
  # coarse woody debris.
  transect_data <- c(
    "transects", "duff_litter_fbd", "vegetation", "woody_species"
  )

  transects <- sections |>
    (`[`)(transect_data) |>
    purrr::reduce(dplyr::left_join, by = "transect") |>
    dplyr::mutate(
      sections$site_data[c("site", "treatment")],
      .before = corner
    ) |>
    dplyr::select(-transect)

  coarse_woody_debris <- sections$coarse_woody_debris |>
    dplyr::mutate(
      sections$site_data[c("site", "treatment")],
      .after = transect
    ) |>
    dplyr::left_join(sections$transects[c("transect", "corner", "azi")]) |>
    dplyr::select(c(site, treatment, corner, azi, dia, decay))

  # Final output with three tables. These will be combined with corresponding
  # tables from other datasheets.
  list(
    plots = sections$site_data,
    transects = transects,
    coarse_woody = coarse_woody_debris
  )
}

data_dir <- "../data"

# Combine fuels data for each plot
#
# This function expects all fuel datasheets to begin with "fuel" and end with
# "csv". It loads all matching files in a given folder and returns the same
# tables as `wrangle_datasheet`, but for all plots combined.

combine_fuels_datasheets <- function(data_dir) {
  files <- list.files(data_dir, pattern = "^fuel.*csv$", full.names = TRUE)
  sheets_list <- purrr::map(files, wrangle_datasheet)
  table_names <- purrr::set_names(names(sheets_list[[1]]))
  purrr::map(table_names, \(x) purrr::list_rbind(purrr::map(sheets_list, x)))
}
