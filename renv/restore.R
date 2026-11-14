# Run with Rscript --vanilla renv/restore.R, from the repository root.
# Avoid activating a partially restored library before repairing it.
if (!file.exists("renv.lock")) stop("Run from the repository root.")
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("renv", quietly = TRUE)) {
  bootstrap <- file.path("renv", "bootstrap")
  dir.create(bootstrap, recursive = TRUE, showWarnings = FALSE)
  install.packages("renv", lib = bootstrap)
  .libPaths(c(normalizePath(bootstrap), .libPaths()))
}
# Explicit library is essential with --vanilla: the active library is otherwise
# the user's system library, not this project's library.
renv::restore(project = getwd(), library = renv::paths$library(project = getwd()),
              prompt = FALSE)
