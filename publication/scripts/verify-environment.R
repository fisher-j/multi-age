# Shared runtime check: inspect actual namespaces and readable DESCRIPTION files.
verify_publication_environment <- function() {
  root <- normalizePath(getwd(), winslash = "/")
  stopifnot(getRversion() == "4.6.2", file.exists("renv.lock"),
            grepl(paste0(root, "/renv/library/"),
                  normalizePath(.libPaths()[1], winslash = "/"), fixed = TRUE),
            !any(grepl("analysis/renv", .libPaths(), fixed = TRUE)))
  packages <- trimws(strsplit(gsub("[\r\n]", "", read.dcf("DESCRIPTION")[1, "Imports"]), ",")[[1]])
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Required namespaces unavailable: ", paste(missing, collapse = ", "))
  lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)
  for (package in names(lock$Packages)) {
    descriptions <- file.path(.libPaths(), package, "DESCRIPTION")
    descriptions <- descriptions[file.exists(descriptions)]
    version <- if (length(descriptions)) unname(read.dcf(descriptions[1], fields = "Version")[1, 1]) else NA_character_
    if (!identical(version, lock$Packages[[package]]$Version)) stop("Package differs from publication lock: ", package)
  }
  invisible(TRUE)
}
if (sys.nframe() == 0L) {
  source(".Rprofile")
  verify_publication_environment()
}
