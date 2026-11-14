pull_nfl <- function() {
  cache_rds(path_in("cache", "raw", "nfl_schedules.rds"), {
    say("Downloading NFL schedules with nflreadr::load_schedules")
    seasons <- 1999:2026
    raw <- tryCatch(
      nflreadr::load_schedules(seasons = seasons),
      error = function(e) {
        say("NFL 2026 schedule was not available (", conditionMessage(e), "); stopping at 2025")
        nflreadr::load_schedules(seasons = 1999:2025)
      }
    )
    needed <- c(
      "game_id", "season", "game_type", "week", "gameday", "home_team", "away_team",
      "home_score", "away_score", "location", "stadium", "stadium_id"
    )
    missing <- setdiff(needed, names(raw))
    if (length(missing)) {
      stop("nflverse schedule is missing columns: ", paste(missing, collapse = ", "))
    }
    raw
  })
}

tidy_nfl <- function(raw) {
  # Odds and spread columns travel with this file. They are not copied forward.
  reg <- raw$game_type == "REG"
  home_score <- suppressWarnings(as.numeric(raw$home_score))
  away_score <- suppressWarnings(as.numeric(raw$away_score))
  keep <- reg & is.finite(home_score) & is.finite(away_score)
  df <- raw[keep, , drop = FALSE]
  season_start <- as.integer(df$season)
  home_fr <- require_mapped("NFL", df$home_team, map_abbrev("NFL", df$home_team), season_start, "tidy_nfl")
  away_fr <- require_mapped("NFL", df$away_team, map_abbrev("NFL", df$away_team), season_start, "tidy_nfl")
  data.frame(
    league = "NFL",
    game_id = paste0("NFL-", df$game_id),
    game_date = parse_date(df$gameday),
    season_start = season_start,
    season_end = season_start + 1L,
    week = as.integer(df$week),
    home_abbr = as.character(df$home_team),
    away_abbr = as.character(df$away_team),
    home_franchise = home_fr,
    away_franchise = away_fr,
    venue = as.character(df$stadium),
    venue_city = NA_character_,
    venue_region = NA_character_,
    venue_key = paste0("NFL-", df$stadium_id),
    home_score = as.numeric(df$home_score),
    away_score = as.numeric(df$away_score),
    feed_neutral = df$location == "Neutral",
    attendance = NA_real_,
    source_home_rest = if ("home_rest" %in% names(df)) as.numeric(df$home_rest) else NA_real_,
    source_away_rest = if ("away_rest" %in% names(df)) as.numeric(df$away_rest) else NA_real_,
    venue_tz_feed = NA_character_,
    source = "nflreadr::load_schedules (nflverse, CC BY 4.0)",
    stringsAsFactors = FALSE
  )
}
