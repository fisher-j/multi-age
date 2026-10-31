Sys.setenv(RENV_PATHS_CACHE = file.path(getwd(), "renv", "cache"),
           RENV_PATHS_ROOT = file.path(getwd(), "renv", "local"),
           RENV_CONFIG_CACHE_ENABLED = "FALSE")
# Runtime checks use actual namespace loads and lockfile versions. Avoid the
# broad startup scan, which is unreliable under Windows sandbox permissions.
options(renv.config.synchronized.check = FALSE,
        renv.config.user.profile = FALSE,
        renv.config.sandbox.enabled = FALSE)
source("renv/activate.R")
