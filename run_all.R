# Rebuild the home-edge project from this repository root.
# Usage, from the repository root:
#   Rscript run_all.R
# Set CHARTS_ONLY=1 to redraw figures from outputs/tables without refitting.
# Set FORCE_REFRESH=1 to ignore cached downloads.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg)) {
  root <- dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  root <- normalizePath(getwd())
}
if (!file.exists(file.path(root, "run_all.R"))) {
  stop("Run this script from the repository root.")
}
options(home_edge_root = root)
setwd(root)

if (file.exists("renv/activate.R") && Sys.getenv("HOME_EDGE_NO_RENV", "") != "1") {
  source("renv/activate.R")
}

charts_only <- Sys.getenv("CHARTS_ONLY", "") == "1"
packages <- c("dplyr", "ggplot2", "readr", "ggrepel")
if (!charts_only) {
  packages <- c(packages, "jsonlite", "curl", "hoopR", "nflreadr", "baseballr", "fastRhockey")
}
for (pkg in packages) {
  tryCatch(
    suppressPackageStartupMessages(library(pkg, character.only = TRUE)),
    error = function(e) stop(
      "Cannot load ", pkg, ": ", conditionMessage(e),
      "\nRun `make restore` from the repository root, then restart R.",
      call. = FALSE
    )
  )
}

source("R/config.R")
source("R/utils.R")
source("R/model.R")
source("R/franchises.R")
source("R/travel.R")
source("R/crowd.R")
source("R/pull_nba.R")
source("R/pull_nfl.R")
source("R/pull_mlb.R")
source("R/pull_nhl.R")
source("R/context.R")
source("R/analyze.R")
source("R/charts.R")
source("R/summary.R")

options(timeout = max(getOption("timeout"), 600))
started <- Sys.time()

write_public_nfl <- function(games) {
  keep <- c(
    "league", "game_id", "game_date", "season_start", "season_end", "season_complete",
    "week", "home_franchise", "away_franchise", "home_abbr", "away_abbr",
    "venue", "venue_place", "site_class", "home_score", "away_score", "margin", "home_win",
    "feed_neutral", "crowd", "crowd_detail",
    "home_prev_date", "away_prev_date", "home_prev_venue", "away_prev_venue",
    "home_days_rest", "away_days_rest", "home_rest_location", "away_rest_location",
    "home_b2b", "away_b2b", "home_stand_game", "away_trip_game",
    "home_miles", "away_miles", "home_tz_cross", "away_tz_cross",
    "home_eastward", "away_eastward", "denver_utah", "high_elevation",
    "neutral_margin", "expected_margin", "p_neutral", "p_home", "h_pregame", "sigma_pregame",
    "leftover_margin", "leftover_wp", "source"
  )
  nfl <- games[games$league == "NFL", intersect(keep, names(games)), drop = FALSE]
  dest <- path_in("data", "processed", "nfl_games.csv")
  ensure_dir(dirname(dest))
  readr::write_csv(nfl, dest)
  say("Wrote ", dest, " (", nrow(nfl), " regular-season games, CC BY 4.0 via nflverse)")
}

write_versions <- function() {
  pkgs <- c(
    "dplyr", "ggplot2", "readr", "ggrepel", "jsonlite", "curl",
    "hoopR", "nflreadr", "baseballr", "fastRhockey", "testthat"
  )
  info <- data.frame(
    package = pkgs,
    version = vapply(pkgs, function(pkg) as.character(utils::packageVersion(pkg)), character(1)),
    stringsAsFactors = FALSE
  )
  readr::write_csv(info, path_in("outputs", "package_versions.csv"))
}

if (charts_only) {
  say("Redrawing charts from existing tables")
  run_charts()
  write_results()
  quit(save = "no", status = 0)
}

say("Pulling schedules")
nba <- tidy_nba(pull_nba())
nfl <- tidy_nfl(pull_nfl())
mlb <- pull_mlb()
nhl <- tidy_nhl(pull_nhl())
games <- dplyr::bind_rows(nba, nfl, mlb, nhl)
games <- games[!duplicated(games$game_id), , drop = FALSE]
missing_date <- is.na(games$game_date)
if (any(missing_date)) say("Dropping ", sum(missing_date), " games with no parseable date")
games <- games[!missing_date, , drop = FALSE]
future <- games$game_date > study_as_of
if (any(future)) say("Dropping ", sum(future), " games dated after ", study_as_of)
games <- games[!future, , drop = FALSE]
# Scores from suspended games are not observable until their completion date.
if (!"outcome_date" %in% names(games)) games$outcome_date <- games$game_date
games$outcome_date[is.na(games$outcome_date)] <- games$game_date[is.na(games$outcome_date)]
unavailable <- games$outcome_date > study_as_of
if (any(unavailable)) say("Dropping ", sum(unavailable), " results not final as of ", study_as_of)
games <- games[!unavailable, , drop = FALSE]
# A 0-0 final score in these feeds is an unplayed placeholder, not a tie.
placeholder <- games$home_score == 0 & games$away_score == 0
if (any(placeholder)) say("Dropping ", sum(placeholder), " games scored 0-0")
games <- games[!placeholder, , drop = FALSE]
games <- games[order(games$league, games$game_date, games$game_id), ]
say("Games in hand: ", nrow(games))

say("Attaching venues, rest, and travel")
games <- prepare_games(games)
say("Fitting pregame ratings")
games <- fit_games(games)

ensure_dir(path_in("cache", "processed"))
saveRDS(games, path_in("cache", "processed", "games.rds"))
write_public_nfl(games)

say("Estimating home edge")
run_analysis(games)
say("Drawing charts")
run_charts()
write_results()
write_versions()

elapsed <- difftime(Sys.time(), started, units = "mins")
say(sprintf("Finished in %.1f minutes", as.numeric(elapsed)))
