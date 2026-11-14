# NHL schedules.
# fastRhockey::load_nhl_schedule reads the package's schedule release, built
# from the NHL API at api-web.nhle.com. That release omits neutralSite and
# venue time zone, so those two fields are read from the same club-schedule
# endpoint fastRhockey::nhl_schedule() calls, and cached under cache/raw/nhl.

nhl_team_codes <- function() {
  c(
    "ANA", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM",
    "FLA", "LAK", "MIN", "MTL", "NJD", "NSH", "NYI", "NYR", "OTT", "PHI", "PIT",
    "SEA", "SJS", "STL", "TBL", "TOR", "UTA", "VAN", "VGK", "WPG", "WSH",
    "ATL", "PHX", "ARI"
  )
}

pull_nhl <- function() {
  cache_rds(path_in("cache", "raw", "nhl_schedule.rds"), {
    release <- load_nhl_release()
    api <- fetch_nhl_api_games(release)
    list(release = release, api = api)
  })
}

load_nhl_release <- function() {
  say("Downloading NHL schedules with fastRhockey::load_nhl_schedule")
  # The loader keys seasons by the ending year. 2011 = 2010-11.
  seasons <- 2011:2027
  raw <- tryCatch(
    fastRhockey::load_nhl_schedule(seasons = seasons),
    error = function(e) {
      say("NHL release request failed (", conditionMessage(e), "); retrying through ", max(seasons) - 1L)
      fastRhockey::load_nhl_schedule(seasons = seasons[-length(seasons)])
    }
  )
  needed <- c("game_id", "game_type", "game_date", "home_team_abbr", "away_team_abbr", "home_score", "away_score", "venue")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    stop("fastRhockey schedule is missing columns: ", paste(missing, collapse = ", "))
  }
  raw
}

fetch_nhl_api_games <- function(release) {
  end_years <- sort(unique(nhl_end_year(release$season, release$season_full)))
  # Warmup seasons are not in the release (it starts at ending year 2011).
  end_years <- sort(unique(c(2009L, 2010L, end_years)))
  pieces <- list()
  for (end_year in end_years) {
    season_str <- sprintf("%d%d", end_year - 1L, end_year)
    say("NHL API season ", season_str)
    teams <- nhl_team_codes()
    in_season <- nhl_end_year(release$season, release$season_full) == end_year
    if (any(in_season, na.rm = TRUE)) {
      teams <- unique(c(teams, release$home_team_abbr[in_season], release$away_team_abbr[in_season]))
    }
    teams <- teams[!is.na(teams) & teams != ""]
    for (team in teams) {
      parsed <- fetch_nhl_club(team, season_str, end_year)
      if (!is.null(parsed) && nrow(parsed)) pieces[[length(pieces) + 1L]] <- parsed
    }
  }
  if (!length(pieces)) {
    say("NHL API enrichment returned no games; neutral-site and time-zone fields will be missing")
    return(empty_api_games())
  }
  out <- do.call(rbind, pieces)
  out <- out[!duplicated(out$game_id), , drop = FALSE]
  say("NHL API enrichment kept ", nrow(out), " unique games")
  out
}

empty_api_games <- function() {
  data.frame(
    game_id = character(),
    season_full = character(),
    game_type = character(),
    game_date = as.Date(character()),
    home_team_abbr = character(),
    away_team_abbr = character(),
    home_score = numeric(),
    away_score = numeric(),
    venue = character(),
    game_state = character(),
    season = integer(),
    neutral_site = logical(),
    venue_tz = character(),
    stringsAsFactors = FALSE
  )
}

fetch_nhl_club <- function(team, season_str, end_year) {
  dest <- path_in("cache", "raw", "nhl", paste0(team, "_", season_str, ".json"))
  url <- sprintf("https://api-web.nhle.com/v1/club-schedule-season/%s/%s", team, season_str)
  if (!file.exists(dest) || file.info(dest)$size < 50 || Sys.getenv("FORCE_REFRESH", "") == "1") {
    ensure_dir(dirname(dest))
    handle <- curl::new_handle(useragent = user_agent())
    resp <- tryCatch(curl::curl_fetch_disk(url, dest, handle = handle), error = function(e) NULL)
    if (is.null(resp) || resp$status_code >= 400L) {
      if (file.exists(dest)) unlink(dest)
      return(NULL)
    }
    Sys.sleep(0.04)
  }
  raw <- tryCatch(jsonlite::fromJSON(dest, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(raw) || is.null(raw$games) || !length(raw$games)) return(NULL)
  games <- raw$games
  if (!is.data.frame(games)) return(NULL)
  if (!nrow(games)) return(NULL)
  home <- nested_field(games, "homeTeam", "abbrev")
  away <- nested_field(games, "awayTeam", "abbrev")
  home_score <- suppressWarnings(as.numeric(nested_field(games, "homeTeam", "score")))
  away_score <- suppressWarnings(as.numeric(nested_field(games, "awayTeam", "score")))
  venue <- nested_field(games, "venue", "default")
  if (all(is.na(venue)) && "venue" %in% names(games) && is.character(games$venue)) venue <- games$venue
  neutral <- if ("neutralSite" %in% names(games)) as.logical(games$neutralSite) else FALSE
  tz <- if ("venueTimezone" %in% names(games)) as.character(games$venueTimezone) else NA_character_
  game_type <- ifelse(
    games$gameType == 2, "R",
    ifelse(games$gameType == 3, "P", ifelse(games$gameType == 1, "PR", as.character(games$gameType)))
  )
  data.frame(
    game_id = as.character(games$id),
    season_full = as.character(games$season),
    game_type = game_type,
    game_date = parse_date(games$gameDate),
    home_team_abbr = home,
    away_team_abbr = away,
    home_score = home_score,
    away_score = away_score,
    venue = venue,
    game_state = as.character(games$gameState),
    season = as.integer(end_year),
    neutral_site = neutral %in% TRUE,
    venue_tz = tz,
    stringsAsFactors = FALSE
  )
}

nested_field <- function(df, parent, child) {
  if (!parent %in% names(df)) return(rep(NA_character_, nrow(df)))
  value <- df[[parent]]
  if (is.data.frame(value) && child %in% names(value)) return(as.character(value[[child]]))
  rep(NA_character_, nrow(df))
}

nhl_end_year <- function(season, season_full) {
  s <- suppressWarnings(as.integer(season))
  full <- as.character(season_full)
  from_full <- suppressWarnings(as.integer(substr(full, 5, 8)))
  ifelse(!is.na(s) & s >= 1990 & s <= 2100, s, from_full)
}

tidy_nhl <- function(raw) {
  release <- raw$release
  api <- raw$api
  keep_cols <- c(
    "game_id", "game_type", "game_date", "home_team_abbr", "away_team_abbr",
    "home_score", "away_score", "venue", "season", "season_full"
  )
  release <- release[, intersect(keep_cols, names(release)), drop = FALSE]
  release$game_date <- parse_date(release$game_date)
  api_ids <- as.character(api$game_id)
  release$game_id <- as.character(release$game_id)
  flags <- api[!duplicated(api_ids) & api$game_type == "R", c("game_id", "neutral_site", "venue_tz")]
  names(flags)[2:3] <- c("api_neutral", "api_tz")
  flags$game_id <- as.character(flags$game_id)
  df <- merge(release, flags, by = "game_id", all.x = TRUE, sort = FALSE)
  have <- as.character(df$game_id)
  extra <- api[api$game_type == "R" & !api_ids %in% have, , drop = FALSE]
  if (nrow(extra)) {
    say("Adding ", nrow(extra), " NHL games found only on the NHL API")
    extra_df <- data.frame(
      game_id = as.character(extra$game_id),
      game_type = extra$game_type,
      game_date = extra$game_date,
      home_team_abbr = extra$home_team_abbr,
      away_team_abbr = extra$away_team_abbr,
      home_score = extra$home_score,
      away_score = extra$away_score,
      venue = extra$venue,
      season = extra$season,
      season_full = extra$season_full,
      api_neutral = extra$neutral_site,
      api_tz = extra$venue_tz,
      stringsAsFactors = FALSE
    )
    for (col in setdiff(names(df), names(extra_df))) extra_df[[col]] <- NA
    df <- rbind(df, extra_df[, names(df), drop = FALSE])
  }
  reg <- df$game_type == "R"
  home_score <- suppressWarnings(as.numeric(df$home_score))
  away_score <- suppressWarnings(as.numeric(df$away_score))
  keep <- reg & is.finite(home_score) & is.finite(away_score)
  df <- df[keep, , drop = FALSE]
  home_score <- home_score[keep]
  away_score <- away_score[keep]
  season_end <- nhl_end_year(df$season, if ("season_full" %in% names(df)) df$season_full else NA)
  season_start <- season_end - 1L
  home_fr <- require_mapped("NHL", df$home_team_abbr, map_abbrev("NHL", df$home_team_abbr), season_start, "tidy_nhl")
  away_fr <- require_mapped("NHL", df$away_team_abbr, map_abbrev("NHL", df$away_team_abbr), season_start, "tidy_nhl")
  venue <- as.character(df$venue)
  feed_neutral <- if ("api_neutral" %in% names(df)) df$api_neutral %in% TRUE else FALSE
  data.frame(
    league = "NHL",
    game_id = paste0("NHL-", df$game_id),
    game_date = parse_date(df$game_date),
    season_start = season_start,
    season_end = season_end,
    week = NA_integer_,
    home_abbr = as.character(df$home_team_abbr),
    away_abbr = as.character(df$away_team_abbr),
    home_franchise = home_fr,
    away_franchise = away_fr,
    venue = venue,
    venue_city = NA_character_,
    venue_region = NA_character_,
    venue_key = paste0("NHL-", tolower(venue)),
    home_score = home_score,
    away_score = away_score,
    feed_neutral = feed_neutral,
    attendance = NA_real_,
    source_home_rest = NA_real_,
    source_away_rest = NA_real_,
    venue_tz_feed = if ("api_tz" %in% names(df)) as.character(df$api_tz) else NA_character_,
    source = "fastRhockey::load_nhl_schedule and api-web.nhle.com club schedules",
    stringsAsFactors = FALSE
  )
}
