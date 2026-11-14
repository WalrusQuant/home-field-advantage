# Retrosheet game logs are the MLB source through the last published season.
# baseballr::mlb_schedule (MLB Stats API) fills any later season the logs do
# not cover yet. Game-level rows are cached and are not committed.

neutral_park_ids <- function() {
  c("LON01", "MEX02", "SEO01", "BIR01", "WIL02", "TOK01", "SYD01", "SYD02")
}

pull_mlb <- function() {
  cache_rds(path_in("cache", "raw", "mlb_games.rds"), {
    parks <- load_retrosheet_parks()
    pieces <- list()
    for (year in 1970:2026) {
      parsed <- read_retrosheet_year(year)
      if (is.null(parsed)) next
      pieces[[length(pieces) + 1L]] <- parsed
    }
    have <- vapply(pieces, function(x) x$season_start[1], integer(1))
    missing_years <- setdiff(2005:2026, have)
    for (year in missing_years) {
      say("Retrosheet has no ", year, " game log; trying baseballr::mlb_schedule")
      api <- read_mlb_api_year(year)
      if (!is.null(api)) pieces[[length(pieces) + 1L]] <- api
    }
    if (!length(pieces)) stop("No MLB games were read.")
    out <- do.call(rbind, pieces)
    attr(out, "parks_note") <- "Retrosheet ballpark list used only to attach city and park name."
    out
  })
}

load_retrosheet_parks <- function() {
  dest <- path_in("cache", "raw", "mlb", "ballparks.zip")
  download_file("https://www.retrosheet.org/ballparks.zip", dest)
  tmp <- path_in("cache", "raw", "mlb", "parks_unzip")
  files <- utils::unzip(dest, exdir = tmp)
  csv <- files[grepl("\\.csv$", files, ignore.case = TRUE)][1]
  parks <- read.csv(csv, stringsAsFactors = FALSE)
  names(parks) <- tolower(names(parks))
  parks
}

read_retrosheet_year <- function(year) {
  dest <- path_in("cache", "raw", "mlb", sprintf("gl%d.zip", year))
  url <- sprintf("https://www.retrosheet.org/gamelogs/gl%d.zip", year)
  ensure_dir(dirname(dest))
  if (!file.exists(dest) || file.info(dest)$size == 0 || Sys.getenv("FORCE_REFRESH", "") == "1") {
    handle <- curl::new_handle(useragent = user_agent())
    resp <- curl::curl_fetch_disk(url, dest, handle = handle)
    if (resp$status_code == 404L) {
      if (file.exists(dest)) unlink(dest)
      say("Retrosheet game log ", year, " is not published (HTTP 404)")
      return(NULL)
    }
    if (resp$status_code >= 400L) {
      if (file.exists(dest)) unlink(dest)
      stop("Retrosheet download failed for ", year, " with HTTP ", resp$status_code)
    }
  }
  tmp <- path_in("cache", "raw", "mlb", paste0("unzip-", year))
  files <- utils::unzip(dest, exdir = tmp)
  txt <- files[grepl("gl[0-9]+\\.txt$", files, ignore.case = TRUE)][1]
  if (is.na(txt)) stop("No game-log text file inside ", dest)
  raw <- readr::read_csv(
    txt,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    na = c("", "NA"),
    progress = FALSE,
    show_col_types = FALSE
  )
  if (ncol(raw) < 18L) stop("Retrosheet ", year, " has ", ncol(raw), " columns; expected at least 18")
  vis_league <- raw[[5]]
  home_league <- raw[[8]]
  keep <- vis_league %in% c("AL", "NL") & home_league %in% c("AL", "NL")
  forfeit <- toupper(trimws(raw[[15]]))
  keep <- keep & !(forfeit %in% c("V", "H", "T"))
  raw <- raw[keep, , drop = FALSE]
  home_score <- suppressWarnings(as.numeric(raw[[11]]))
  away_score <- suppressWarnings(as.numeric(raw[[10]]))
  played <- is.finite(home_score) & is.finite(away_score)
  raw <- raw[played, , drop = FALSE]
  home_score <- home_score[played]
  away_score <- away_score[played]
  att <- suppressWarnings(as.numeric(raw[[18]]))
  # Retrosheet uses 0 when attendance is unknown. 2020 is the exception:
  # the regular season was played without spectators, and the logs are 0.
  if (year != 2020L) att[!is.na(att) & att == 0] <- NA_real_
  parks <- load_retrosheet_parks()
  park_id <- raw[[17]]
  park_name <- parks$name[match(park_id, parks$parkid)]
  park_city <- parks$city[match(park_id, parks$parkid)]
  park_region <- parks$state[match(park_id, parks$parkid)]
  season_start <- rep(as.integer(year), nrow(raw))
  home_fr <- require_mapped("MLB", raw[[7]], map_abbrev("MLB", raw[[7]]), season_start, paste("retrosheet", year))
  away_fr <- require_mapped("MLB", raw[[4]], map_abbrev("MLB", raw[[4]]), season_start, paste("retrosheet", year))
  # Pre-warmup seasons may contain an unmapped code. Keep the raw code so the
  # long home-win series still counts the game; the model window will have stopped above.
  home_fr <- ifelse(is.na(home_fr), paste0("UNK-", toupper(raw[[7]])), home_fr)
  away_fr <- ifelse(is.na(away_fr), paste0("UNK-", toupper(raw[[4]])), away_fr)
  data.frame(
    league = "MLB",
    game_id = paste("MLB", raw[[1]], raw[[7]], raw[[4]], raw[[2]], sep = "-"),
    game_date = parse_date(raw[[1]]),
    season_start = season_start,
    season_end = season_start,
    week = NA_integer_,
    home_abbr = toupper(raw[[7]]),
    away_abbr = toupper(raw[[4]]),
    home_franchise = home_fr,
    away_franchise = away_fr,
    venue = ifelse(is.na(park_name), park_id, park_name),
    venue_city = park_city,
    venue_region = park_region,
    venue_key = paste0("MLB-", park_id),
    home_score = home_score,
    away_score = away_score,
    feed_neutral = park_id %in% neutral_park_ids(),
    attendance = att,
    source_home_rest = NA_real_,
    source_away_rest = NA_real_,
    venue_tz_feed = NA_character_,
    source = "Retrosheet game logs (www.retrosheet.org)",
    stringsAsFactors = FALSE
  )
}

read_mlb_api_year <- function(year) {
  raw <- tryCatch(
    baseballr::mlb_schedule(season = year, level_ids = "1"),
    error = function(e) {
      say("baseballr::mlb_schedule failed for ", year, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(raw) || !nrow(raw)) return(NULL)
  needed <- c(
    "game_type", "official_date", "game_pk", "teams_home_score", "teams_away_score",
    "teams_home_team_name", "teams_away_team_name", "venue_name", "status_abstract_game_state"
  )
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    stop("baseballr::mlb_schedule is missing columns: ", paste(missing, collapse = ", "))
  }
  keep <- raw$game_type == "R" & raw$status_abstract_game_state == "Final"
  df <- raw[keep, , drop = FALSE]
  home_score <- suppressWarnings(as.numeric(df$teams_home_score))
  away_score <- suppressWarnings(as.numeric(df$teams_away_score))
  played <- is.finite(home_score) & is.finite(away_score)
  df <- df[played, , drop = FALSE]
  home_score <- home_score[played]
  away_score <- away_score[played]
  season_start <- rep(as.integer(year), nrow(df))
  home_fr <- map_mlb_name(df$teams_home_team_name)
  away_fr <- map_mlb_name(df$teams_away_team_name)
  home_fr <- require_mapped("MLB", df$teams_home_team_name, home_fr, season_start, "mlb_schedule")
  away_fr <- require_mapped("MLB", df$teams_away_team_name, away_fr, season_start, "mlb_schedule")
  game_number <- if ("game_number" %in% names(df)) df$game_number else "1"
  venue_id <- if ("venue_id" %in% names(df)) as.character(df$venue_id) else NA_character_
  data.frame(
    league = "MLB",
    game_id = paste("MLBAPI", df$game_pk, game_number, sep = "-"),
    game_date = parse_date(df$official_date),
    season_start = season_start,
    season_end = season_start,
    week = NA_integer_,
    home_abbr = as.character(df$teams_home_team_name),
    away_abbr = as.character(df$teams_away_team_name),
    home_franchise = home_fr,
    away_franchise = away_fr,
    venue = as.character(df$venue_name),
    venue_city = NA_character_,
    venue_region = NA_character_,
    venue_key = paste0("MLBAPI-", venue_id),
    home_score = home_score,
    away_score = away_score,
    feed_neutral = FALSE,
    attendance = NA_real_,
    source_home_rest = NA_real_,
    source_away_rest = NA_real_,
    venue_tz_feed = NA_character_,
    source = "baseballr::mlb_schedule (MLB Stats API)",
    stringsAsFactors = FALSE
  )
}
