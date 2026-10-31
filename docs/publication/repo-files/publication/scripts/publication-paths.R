# Shared I/O boundary; independent of thesis startup files and working directory.
if (getRversion() != "4.6.2")
  stop("Publication calculations require R 4.6.2. Use run-publication.bat from the repository root.")

publication_paths <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  staged <- Sys.getenv("PUBLICATION_OUTPUT_ROOT")
  output <- if (nzchar(staged)) staged else file.path(root, "outputs")
  assets <- if (nzchar(staged)) file.path(dirname(output), "assets") else file.path(root, "assets")
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  dir.create(assets, recursive = TRUE, showWarnings = FALSE)
  list(root = root, input = file.path(root, "inputs", "thesis"),
       output = normalizePath(output, winslash = "/"), assets = assets)
}

# The current publication specification requires unconditional simulation;
# the locked DHARMa version supports this argument explicitly.
publication_simulate_residuals <- function(fittedModel, ...) {
  DHARMa::simulateResiduals(fittedModel, simulateREs = "unconditional", ...)
}
