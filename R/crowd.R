# Crowd labels.
#
# NFL 2020 is not in the nflverse schedule as attendance. The labels below
# encode the regular-season policy summarized in the "spectators" table of
# https://en.wikipedia.org/wiki/2020_NFL_season (retrieved 2026-11-14), which
# compiles team and league announcements. Playoff crowds in that table are
# ignored. Family-only crowds are coded no_fans, matching the table's count of
# games "with spectators." Week numbers are NFL weeks. home_index is the
# team's nth designated home game that week, skipping neutral-site games.

nfl_2020_crowd <- function(franchise, week, home_index) {
  franchise <- as.character(franchise)
  week <- suppressWarnings(as.integer(week))
  home_index <- suppressWarnings(as.integer(home_index))
  n <- length(franchise)
  crowd <- rep("unknown", n)
  detail <- rep(NA_character_, n)
  no_fans <- c("BUF", "CHI", "DET", "GB", "LV", "LAC", "LAR", "MIN", "NE", "NYG", "NYJ", "SF", "SEA")
  reduced_all <- c("CLE", "DAL", "IND", "JAX", "KC", "MIA")
  opener_closed <- c("CAR", "CIN", "HOU", "TB", "TEN")

  for (i in seq_len(n)) {
    fr <- franchise[i]
    w <- week[i]
    h <- home_index[i]
    if (fr %in% no_fans) {
      crowd[i] <- "no_fans"
      detail[i] <- "no regular-season spectators"
    } else if (fr %in% reduced_all) {
      crowd[i] <- "reduced"
      detail[i] <- "reduced capacity"
    } else if (fr == "ARI") {
      crowd[i] <- if (!is.na(h) && h %in% 4:5) "reduced" else "no_fans"
      detail[i] <- "first three and last three home games closed; two limited games in between"
    } else if (fr == "ATL") {
      if (!is.na(w) && w >= 4) {
        crowd[i] <- "reduced"
      } else if (!is.na(w) && w == 3) {
        crowd[i] <- "no_fans"
        detail[i] <- "family_only"
      } else {
        crowd[i] <- "no_fans"
      }
    } else if (fr == "BAL") {
      crowd[i] <- if (!is.na(w) && w == 8) "reduced" else "no_fans"
      detail[i] <- "one limited game in week 8"
    } else if (fr %in% opener_closed) {
      crowd[i] <- if (!is.na(h) && h == 1L) "no_fans" else "reduced"
      detail[i] <- "home opener closed; later home games reduced"
    } else if (fr == "DEN") {
      if (!is.na(h) && h == 1L) {
        crowd[i] <- "no_fans"
        detail[i] <- "family_only"
      } else if (!is.na(h) && h %in% 2:5) {
        crowd[i] <- "reduced"
      } else {
        crowd[i] <- "no_fans"
      }
    } else if (fr == "NO") {
      crowd[i] <- if (!is.na(w) && w >= 7) "reduced" else "no_fans"
      detail[i] <- "spectators from week 7, reduced"
    } else if (fr == "PHI") {
      crowd[i] <- if (!is.na(w) && w >= 6 && w <= 11) "reduced" else "no_fans"
    } else if (fr == "PIT") {
      crowd[i] <- if (!is.na(w) && w >= 5 && w <= 10) "reduced" else "no_fans"
    } else if (fr == "WAS") {
      crowd[i] <- if (!is.na(w) && w == 9) "reduced" else "no_fans"
      detail[i] <- "one limited game in week 9"
    }
  }
  data.frame(crowd = crowd, crowd_detail = detail, stringsAsFactors = FALSE)
}

assign_crowd <- function(games) {
  games$crowd <- "unknown"
  games$crowd_detail <- NA_character_

  nfl <- games$league == "NFL" & games$season_start == 2020L & !games$feed_neutral
  if (any(nfl)) {
    ord <- order(games$home_franchise[nfl], games$game_date[nfl], games$game_id[nfl])
    idx <- which(nfl)[ord]
    home_index <- ave(seq_along(idx), games$home_franchise[idx], FUN = seq_along)
    labelled <- nfl_2020_crowd(games$home_franchise[idx], games$week[idx], home_index)
    games$crowd[idx] <- labelled$crowd
    games$crowd_detail[idx] <- labelled$crowd_detail
  }

  # Venue median attendance in the last full pre-2020 seasons, for NBA and MLB.
  ref <- games$league %in% c("NBA", "MLB") &
    games$site_class == "home" &
    is.finite(games$attendance) &
    games$attendance > 500 &
    ((games$league == "NBA" & games$season_start %in% 2016:2018) |
      (games$league == "MLB" & games$season_start %in% 2017:2019))
  med <- tapply(games$attendance[ref], games$venue_key[ref], stats::median)
  ratio <- games$attendance / unname(med[as.character(games$venue_key)])

  nba <- games$league == "NBA"
  games$crowd[nba & games$bubble] <- "bubble"
  empty_nba <- nba & !games$bubble & is.finite(games$attendance) & games$attendance <= 250
  reduced_nba <- nba & !games$bubble & is.finite(ratio) & ratio < 0.5 & !empty_nba
  normal_nba <- nba & !games$bubble & is.finite(games$attendance) & games$attendance > 250 & !reduced_nba
  games$crowd[empty_nba] <- "no_fans"
  games$crowd[reduced_nba] <- "reduced"
  games$crowd[normal_nba] <- "normal"

  mlb <- games$league == "MLB"
  mlb_2020 <- mlb & games$season_start == 2020L & games$site_class %in% c("home", "makeshift")
  games$crowd[mlb_2020] <- "no_fans"
  games$crowd_detail[mlb_2020] <- "2020 regular season was played without spectators"
  reduced_mlb <- mlb & !mlb_2020 & is.finite(ratio) & ratio < 0.5 & is.finite(games$attendance) & games$attendance > 0
  normal_mlb <- mlb & !mlb_2020 & is.finite(games$attendance) & games$attendance > 0 & !reduced_mlb
  games$crowd[reduced_mlb] <- "reduced"
  games$crowd[normal_mlb] <- "normal"

  nhl_2020 <- games$league == "NHL" & games$season_start == 2020L & games$site_class == "home"
  games$crowd[nhl_2020] <- "restricted_unknown"
  games$crowd_detail[nhl_2020] <- "2020-21 restricted crowds; the NHL feed has no attendance"
  games
}
