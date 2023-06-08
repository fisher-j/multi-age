# Test for duplicated data in data frames that should be unique
# ... is the unquoted unique identifiers
warn_duplicates <- function(data, ...) {
  data_name <- rlang::as_label(enquo(data))
  duplicates <- data |>
    dplyr::count(...) |>
    dplyr::filter(n > 1)

  if(nrow(duplicates) != 0) {
    duplicates <- do.call(sprintf, c(dup, "%s %s %s %s: %s")) |>
      paste(collapse = "\n")
    warning(
      call. = FALSE,
      "Duplicates detected in `", data_name, "`\n", duplicates
    )
  }
}
