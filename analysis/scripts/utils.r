load_quiet <- function(...) {
  for(p in as.list(match.call()[-1])) {
    (suppressWarnings(suppressPackageStartupMessages(eval(p))))
  }
}
