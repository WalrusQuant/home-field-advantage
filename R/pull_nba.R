pull_nba <- function() {
  cache_rds(path_in("cache", "raw", "nba_schedule.rds"), {
    say("Downloading NBA schedules with hoopR::load_nba_schedule")
    # hoopR's ESPN loader keys seasons by the ending year.
    # 2008 = 2007-08 (warmup). 2027 = 2026-27, partial as of the study date.
    seasons <- 2008:2027
    raw <- tryCatch(
      hoopR::load_nba_schedule(seasons = seasons),
      error = function(e) {
        say("NBA season request failed (", conditionMessage(e), "); retrying without the newest season")
        hoopR::load_nba_schedule(seasons = seasons[-length(seasons)])
      }
    )
    needed <- c(
      "game_id", "game_date", "season", "season_type", "type_abbreviation",
      "home_abbreviation", "away_abbreviation", "home_score", "away_score",
      "venue_full_name", "neutral_site", "attendance"
    )
    missing <- setdiff(needed, names(raw))
    if (length(missing)) {
      stop("hoopR schedule is missing columns: ", paste(missing, collapse = ", "))
    }
    raw
  })
}

tidy_nba <- function(raw) {
  reg <- as.integer(raw$season_type) == 2L & raw$type_abbreviation == "STD"
  reg[is.na(reg)] <- FALSE
  df <- raw[reg, , drop = FALSE]
  home_score <- suppressWarnings(as.numeric(df$home_score))
  away_score <- suppressWarnings(as.numeric(df$away_score))
  played <- is.finite(home_score) & is.finite(away_score)
  df <- df[played, , drop = FALSE]
  home_score <- home_score[played]
  away_score <- away_score[played]
  season_end <- as.integer(df$season)
  home_fr <- map_abbrev("NBA", df$home_abbreviation)
  away_fr <- map_abbrev("NBA", df$away_abbreviation)
  season_start <- season_end - 1L
  # All-Star, Rising Stars, and unfilled future opponents are coded as STD.
  # A real franchise that fails the crosswalk shows up dozens of times; stop then.
  unmapped <- is.na(home_fr) | is.na(away_fr)
  if (any(unmapped)) {
    dropped <- unique(c(df$home_abbreviation[is.na(home_fr)], df$away_abbreviation[is.na(away_fr)]))
    counts <- sort(table(c(df$home_abbreviation[is.na(home_fr)], df$away_abbreviation[is.na(away_fr)])), decreasing = TRUE)
    if (any(counts >= 30)) {
      stop("NBA has unmapped abbreviations with many games: ", paste(names(counts)[counts >= 30], collapse = ", "))
    }
    say("Dropping ", sum(unmapped), " NBA games with non-franchise abbreviations: ", paste(dropped, collapse = ", "))
    df <- df[!unmapped, , drop = FALSE]
    home_score <- home_score[!unmapped]
    away_score <- away_score[!unmapped]
    season_end <- season_end[!unmapped]
    season_start <- season_start[!unmapped]
    home_fr <- home_fr[!unmapped]
    away_fr <- away_fr[!unmapped]
  }
  city <- if ("venue_address_city" %in% names(df)) df$venue_address_city else NA_character_
  region <- if ("venue_address_state" %in% names(df)) df$venue_address_state else NA_character_
  venue_id <- if ("venue_id" %in% names(df)) as.character(df$venue_id) else NA_character_
  data.frame(
    league = "NBA",
    game_id = paste0("NBA-", df$game_id),
    game_date = parse_date(df$game_date),
    season_start = season_start,
    season_end = season_end,
    week = NA_integer_,
    home_abbr = as.character(df$home_abbreviation),
    away_abbr = as.character(df$away_abbreviation),
    home_franchise = home_fr,
    away_franchise = away_fr,
    venue = as.character(df$venue_full_name),
    venue_city = as.character(city),
    venue_region = as.character(region),
    venue_key = ifelse(!is.na(venue_id) & venue_id != "", paste0("NBA-", venue_id), paste0("NBA-", df$venue_full_name)),
    home_score = home_score,
    away_score = away_score,
    feed_neutral = as.logical(df$neutral_site) %in% TRUE,
    attendance = suppressWarnings(as.numeric(df$attendance)),
    source_home_rest = NA_real_,
    source_away_rest = NA_real_,
    venue_tz_feed = NA_character_,
    source = "hoopR::load_nba_schedule (ESPN schedules via sportsdataverse)",
    stringsAsFactors = FALSE
  )
}
