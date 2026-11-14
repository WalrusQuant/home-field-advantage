# Site class, rest, travel, and crowd. Runs before the pregame model so the
# home term is fit only on true home games.

add_site_and_travel <- function(games) {
  places <- load_places()
  patterns <- load_venue_patterns()
  ftable <- load_franchise_table()
  games$usual_place <- franchise_place_id(
    games$league, games$home_franchise, games$season_start, ftable
  )
  pat <- pattern_place(games$venue, patterns)
  city_place <- match_place_id(games$venue_city, games$venue_region, places)
  games$venue_place <- ifelse(!is.na(pat$place_id), pat$place_id, city_place)
  games$venue_place <- ifelse(is.na(games$venue_place), games$usual_place, games$venue_place)
  games$pattern_neutral <- pat$force_neutral
  games$bubble <- games$league == "NBA" &
    games$season_start == 2019L &
    games$game_date >= nba_bubble_start &
    games$game_date <= nba_bubble_end

  place_cols <- places[, c("place_id", "lat", "lon", "tz", "elev_ft", "denver_utah", "high_elevation")]
  names(place_cols)[1] <- "venue_place"
  games <- merge(games, place_cols, by = "venue_place", all.x = TRUE, sort = FALSE)
  if ("venue_tz_feed" %in% names(games)) {
    games$tz <- ifelse(!is.na(games$venue_tz_feed) & games$venue_tz_feed != "", games$venue_tz_feed, games$tz)
  }
  usual <- places[, c("place_id", "lat", "lon")]
  names(usual) <- c("usual_place", "usual_lat", "usual_lon")
  games <- merge(games, usual, by = "usual_place", all.x = TRUE, sort = FALSE)
  games$miles_from_usual <- haversine_miles(games$lat, games$lon, games$usual_lat, games$usual_lon)

  modal <- modal_venues(games)
  games <- merge(
    games,
    modal,
    by = c("league", "home_franchise", "season_start"),
    all.x = TRUE,
    sort = FALSE
  )
  games
}

modal_venues <- function(games) {
  use <- !games$feed_neutral & !games$bubble & !games$pattern_neutral
  base <- games[use, c("league", "home_franchise", "season_start", "venue_key", "venue_place", "usual_place")]
  if (!nrow(base)) {
    return(data.frame(
      league = character(), home_franchise = character(), season_start = integer(),
      modal_key = character(), modal_place = character(), makeshift_season = logical()
    ))
  }
  base$cell <- paste(base$league, base$home_franchise, base$season_start, base$venue_key, sep = "|")
  counts <- as.data.frame(table(base$cell), stringsAsFactors = FALSE)
  names(counts) <- c("cell", "n")
  pieces <- do.call(rbind, strsplit(counts$cell, "|", fixed = TRUE))
  counts$league <- pieces[, 1]
  counts$home_franchise <- pieces[, 2]
  counts$season_start <- as.integer(pieces[, 3])
  counts$venue_key <- pieces[, 4]
  place_lookup <- unique(base[, c("league", "home_franchise", "season_start", "venue_key", "venue_place", "usual_place")])
  counts <- merge(counts, place_lookup, by = c("league", "home_franchise", "season_start", "venue_key"), all.x = TRUE)
  counts <- counts[order(counts$league, counts$home_franchise, counts$season_start, -counts$n), ]
  modal <- counts[!duplicated(paste(counts$league, counts$home_franchise, counts$season_start)), ]
  modal$makeshift_season <- flag_makeshift(modal)
  data.frame(
    league = modal$league,
    home_franchise = modal$home_franchise,
    season_start = modal$season_start,
    modal_key = modal$venue_key,
    modal_place = modal$venue_place,
    makeshift_season = modal$makeshift_season,
    stringsAsFactors = FALSE
  )
}

flag_makeshift <- function(modal) {
  out <- rep(FALSE, nrow(modal))
  for (i in seq_len(nrow(modal))) {
    place_i <- modal$venue_place[i]
    usual_i <- modal$usual_place[i]
    if (length(place_i) != 1L || length(usual_i) != 1L || is.na(place_i) || is.na(usual_i)) next
    manual <- isTRUE(modal$league[i] == "MLB" && modal$home_franchise[i] == "TOR" && modal$season_start[i] == 2020L)
    if (isTRUE(place_i == usual_i) && !manual) next
    same <- modal$league == modal$league[i] & modal$home_franchise == modal$home_franchise[i]
    y <- modal$season_start[i]
    before <- modal$venue_place[same & modal$season_start %in% (y - 3L):(y - 1L)]
    after <- modal$venue_place[same & modal$season_start %in% (y + 1L):(y + 3L)]
    before <- before[!is.na(before)]
    after <- after[!is.na(after)]
    stable <- length(before) >= 1L && length(after) >= 1L &&
      all(before == usual_i) && all(after == usual_i)
    out[i] <- isTRUE(stable) || manual
  }
  out
}

classify_sites <- function(games) {
  off <- is.finite(games$miles_from_usual) & games$miles_from_usual > off_campus_miles
  # Unknown geography cannot establish a true home site. Do not use the
  # full-season modal venue to decide home eligibility.
  missing_coord <- !is.finite(games$lat) | !is.finite(games$lon) |
    !is.finite(games$miles_from_usual)
  makeshift_game <- off & !missing_coord & games$makeshift_season %in% TRUE &
    games$venue_key == games$modal_key
  site <- ifelse(games$bubble, "bubble", "home")
  site[site == "home" & (games$feed_neutral | games$pattern_neutral)] <- "neutral"
  site[site == "home" & makeshift_game] <- "makeshift"
  site[site == "home" & (off | missing_coord)] <- "alternate"
  games$site_class <- site
  games$model_neutral <- games$site_class != "home"
  games$season_complete <- games$season_start <= completed_start_year[games$league]
  games$in_long_raw <- (games$league == "NFL" & games$season_start >= long_raw_start["NFL"]) |
    (games$league == "MLB" & games$season_start >= long_raw_start["MLB"])
  games
}

add_rest_travel <- function(games) {
  home <- team_slice(games, "home")
  away <- team_slice(games, "away")
  long <- rbind(home, away)
  long <- long[order(long$league, long$franchise, long$season_start, long$game_date, long$game_id), ]
  long <- add_lags(long)
  home_ctx <- side_context(long[long$side == "home", ], "home")
  away_ctx <- side_context(long[long$side == "away", ], "away")
  games <- merge(games, home_ctx, by = c("league", "game_id"), all.x = TRUE, sort = FALSE)
  games <- merge(games, away_ctx, by = c("league", "game_id"), all.x = TRUE, sort = FALSE)
  games
}

team_slice <- function(games, side) {
  is_home <- side == "home"
  franchise <- if (is_home) games$home_franchise else games$away_franchise
  role <- ifelse(
    games$site_class %in% c("neutral", "bubble"),
    "neutral",
    ifelse(is_home & games$site_class == "home", "home", ifelse(is_home, "neutral", "road"))
  )
  data.frame(
    league = games$league,
    game_id = games$game_id,
    franchise = franchise,
    season_start = games$season_start,
    game_date = games$game_date,
    venue = games$venue,
    lat = games$lat,
    lon = games$lon,
    tz = games$tz,
    role = role,
    side = side,
    stringsAsFactors = FALSE
  )
}

add_lags <- function(long) {
  n <- nrow(long)
  long$prev_date <- as.Date(rep(NA, n))
  long$prev_venue <- NA_character_
  long$prev_role <- NA_character_
  long$prev_lat <- NA_real_
  long$prev_lon <- NA_real_
  long$prev_tz <- NA_character_
  long$stand_game <- NA_integer_
  long$trip_game <- NA_integer_
  group <- paste(long$league, long$franchise, long$season_start)
  for (g in unique(group)) {
    idx <- which(group == g)
    if (length(idx) == 0L) next
    prev <- c(NA, idx[-length(idx)])
    use <- !is.na(prev)
    long$prev_date[idx[use]] <- long$game_date[prev[use]]
    long$prev_venue[idx[use]] <- long$venue[prev[use]]
    long$prev_role[idx[use]] <- long$role[prev[use]]
    long$prev_lat[idx[use]] <- long$lat[prev[use]]
    long$prev_lon[idx[use]] <- long$lon[prev[use]]
    long$prev_tz[idx[use]] <- long$tz[prev[use]]
    tn <- trip_numbers(long$role[idx])
    long$stand_game[idx] <- tn$stand
    long$trip_game[idx] <- tn$trip
  }
  long$days_rest <- as.integer(long$game_date - long$prev_date) - 1L
  long$miles <- haversine_miles(long$prev_lat, long$prev_lon, long$lat, long$lon)
  long$tz_cross <- tz_cross_hours(long$prev_tz, long$prev_date, long$tz, long$game_date)
  long$rest_location <- ifelse(
    is.na(long$prev_role), NA_character_,
    ifelse(long$prev_role == "home", "home", ifelse(long$prev_role == "road", "road", "neutral"))
  )
  long$b2b <- !is.na(long$days_rest) & long$days_rest == 0L
  long$stand_game[long$stand_game == 0L] <- NA_integer_
  long$trip_game[long$trip_game == 0L] <- NA_integer_
  long
}

side_context <- function(long, prefix) {
  out <- data.frame(
    league = long$league,
    game_id = long$game_id,
    prev_date = long$prev_date,
    prev_venue = long$prev_venue,
    days_rest = long$days_rest,
    rest_location = long$rest_location,
    b2b = long$b2b,
    stand_game = long$stand_game,
    trip_game = long$trip_game,
    miles = long$miles,
    tz_cross = long$tz_cross,
    eastward = !is.na(long$tz_cross) & long$tz_cross >= 1,
    stringsAsFactors = FALSE
  )
  names(out)[-(1:2)] <- paste0(prefix, "_", names(out)[-(1:2)])
  out
}

prepare_games <- function(games) {
  games <- add_site_and_travel(games)
  games <- classify_sites(games)
  games <- add_rest_travel(games)
  games <- assign_crowd(games)
  games$margin <- games$home_score - games$away_score
  games$home_win <- ifelse(games$margin > 0, 1, ifelse(games$margin < 0, 0, 0.5))
  games
}
