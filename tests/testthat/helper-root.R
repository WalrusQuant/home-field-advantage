root <- normalizePath(getwd())
for (i in 1:4) {
  if (file.exists(file.path(root, "run_all.R"))) break
  root <- dirname(root)
}
options(home_edge_root = root)
source(file.path(root, "R/config.R"))
source(file.path(root, "R/utils.R"))
source(file.path(root, "R/model.R"))
source(file.path(root, "R/franchises.R"))
source(file.path(root, "R/travel.R"))
source(file.path(root, "R/crowd.R"))
