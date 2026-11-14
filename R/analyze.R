season_label <- function(league, season_start) {
  ifelse(
    league %in% c("NBA", "NHL"),
    sprintf("%d-%02d", season_start, (season_start + 1L) %% 100L),
    as.character(season_start)
  )
}

edge_pair <- function(df, label) {
  margin <- boot_mean(df$leftover_margin, df$home_franchise, paste0(label, "|margin"))
  wp <- boot_mean(df$leftover_wp, df$home_franchise, paste0(label, "|wp"))
  raw <- boot_mean(df$home_win, df$home_franchise, paste0(label, "|raw"))
  data.frame(
    n = margin$n,
    raw_home_win = raw$estimate, raw_lo = raw$lo, raw_hi = raw$hi,
    leftover_margin = margin$estimate, margin_lo = margin$lo, margin_hi = margin$hi,
    leftover_wp = wp$estimate, wp_lo = wp$lo, wp_hi = wp$hi,
    stringsAsFactors = FALSE
  )
}

shrink_one_season <- function(df) {
  x <- df$leftover_margin
  g <- df$home_franchise
  ok <- is.finite(x) & !is.na(g)
  x <- x[ok]
  g <- g[ok]
  if (!length(x)) {
    return(list(mu = NA_real_, tau2 = NA_real_, rows = data.frame()))
  }
  sp <- split(x, g)
  n <- vapply(sp, length, integer(1))
  raw <- vapply(sp, mean, numeric(1))
  vars <- vapply(sp, function(z) if (length(z) > 1L) stats::var(z) else NA_real_, numeric(1))
  s2_den <- sum((n - 1L)[n > 1L])
  s2 <- if (s2_den > 0) sum((n - 1L) * vars, na.rm = TRUE) / s2_den else stats::var(x)
  if (!is.finite(s2) || s2 <= 0) s2 <- stats::var(x)
  mu <- mean(x)
  use <- n >= 8L
  tau2 <- 0
  if (sum(use) >= 4L && is.finite(s2)) {
    center <- stats::weighted.mean(raw[use], n[use])
    var_m <- sum(n[use] * (raw[use] - center)^2) / sum(n[use])
    tau2 <- max(0, var_m - mean(s2 / n[use]))
  }
  shrink <- (s2 / n) / (s2 / n + tau2)
  shrink[!is.finite(shrink)] <- 1
  shrunk <- (1 - shrink) * raw + shrink * mu
  list(
    mu = mu,
    tau2 = tau2,
    rows = data.frame(
      franchise = names(raw),
      n_home = as.integer(n),
      raw_home_win = NA_real_,
      leftover_margin = as.numeric(raw),
      leftover_shrunk = as.numeric(shrunk),
      stringsAsFactors = FALSE
    )
  )
}

joint_for <- function(df) {
  teams <- sort(unique(c(df$home_franchise, df$away_franchise)))
  teams <- teams[!is.na(teams)]
  if (length(teams) < 2L) return(NA_real_)
  ix <- setNames(seq_along(teams), teams)
  joint_home_term(
    unname(ix[df$home_franchise]),
    unname(ix[df$away_franchise]),
    df$margin,
    n_teams = length(teams),
    neutral = df$site_class != "home"
  )
}

run_analysis <- function(games) {
  games$season_label <- season_label(games$league, games$season_start)
  home <- games[games$in_home_edge, , drop = FALSE]
  primary <- home[home$season_complete, , drop = FALSE]

  coverage <- lapply(split(games, paste(games$league, games$season_start)), function(df) {
    data.frame(
      league = df$league[1],
      season_start = df$season_start[1],
      season_label = df$season_label[1],
      season_complete = df$season_complete[1],
      n_games = nrow(df),
      n_home_edge = sum(df$in_home_edge),
      n_neutral = sum(df$site_class == "neutral"),
      n_bubble = sum(df$site_class == "bubble"),
      n_makeshift = sum(df$site_class == "makeshift"),
      n_alternate = sum(df$site_class == "alternate"),
      n_with_attendance = sum(is.finite(df$attendance)),
      n_with_coords = sum(is.finite(df$lat) & is.finite(df$lon)),
      stringsAsFactors = FALSE
    )
  })
  coverage <- do.call(rbind, coverage)
  coverage <- coverage[order(coverage$league, coverage$season_start), ]
  write_table(coverage, "coverage.csv")

  season_rows <- list()
  team_rows <- list()
  leagues <- c("MLB", "NBA", "NFL", "NHL")
  for (lg in leagues) {
    seasons <- sort(unique(home$season_start[home$league == lg]))
    for (yr in seasons) {
      sl <- home[home$league == lg & home$season_start == yr, , drop = FALSE]
      all_sl <- games[games$league == lg & games$season_start == yr & games$season_start >= warmup_start_year, , drop = FALSE]
      stats <- edge_pair(sl, sprintf("%s|%s|season", lg, yr))
      shr <- shrink_one_season(sl)
      iqr <- boot_iqr(shr$rows$leftover_shrunk, sprintf("%s|%s|iqr", lg, yr))
      # Raw home win on the same rows, for the shrunk table.
      if (nrow(shr$rows)) {
        raw_by <- tapply(sl$home_win, sl$home_franchise, mean)
        strength <- tapply(sl$home_strength, sl$home_franchise, mean)
        shr$rows$raw_home_win <- as.numeric(raw_by[shr$rows$franchise])
        shr$rows$mean_home_strength <- as.numeric(strength[shr$rows$franchise])
        shr$rows$league <- lg
        shr$rows$season_start <- yr
        shr$rows$season_label <- season_label(lg, yr)
        shr$rows$season_complete <- sl$season_complete[1]
        team_rows[[length(team_rows) + 1L]] <- shr$rows
      }
      season_rows[[length(season_rows) + 1L]] <- data.frame(
        league = lg,
        season_start = yr,
        season_label = season_label(lg, yr),
        season_complete = sl$season_complete[1],
        unit = unname(scoring_unit[[lg]]),
        n = stats$n,
        raw_home_win = stats$raw_home_win,
        raw_lo = stats$raw_lo,
        raw_hi = stats$raw_hi,
        leftover_margin = stats$leftover_margin,
        margin_lo = stats$margin_lo,
        margin_hi = stats$margin_hi,
        leftover_wp = stats$leftover_wp,
        wp_lo = stats$wp_lo,
        wp_hi = stats$wp_hi,
        team_iqr = iqr$estimate,
        iqr_lo = iqr$lo,
        iqr_hi = iqr$hi,
        joint_home_term = joint_for(all_sl),
        stringsAsFactors = FALSE
      )
    }
  }
  season <- do.call(rbind, season_rows)
  write_table(season, "season_home_edge.csv")
  teams <- do.call(rbind, team_rows)
  write_table(teams, "team_season.csv")

  tier_rows <- list()
  for (lg in leagues) {
    sub <- primary[primary$league == lg & !is.na(primary$tier), , drop = FALSE]
    for (tier in tier_labels) {
      sl <- sub[sub$tier == tier, , drop = FALSE]
      stats <- edge_pair(sl, sprintf("%s|tier|%s", lg, tier))
      tier_rows[[length(tier_rows) + 1L]] <- data.frame(
        league = lg, tier = tier, unit = unname(scoring_unit[[lg]]),
        n = stats$n,
        leftover_margin = stats$leftover_margin, margin_lo = stats$margin_lo, margin_hi = stats$margin_hi,
        leftover_wp = stats$leftover_wp, wp_lo = stats$wp_lo, wp_hi = stats$wp_hi,
        stringsAsFactors = FALSE
      )
    }
  }
  write_table(do.call(rbind, tier_rows), "tier_home_edge.csv")

  write_table(crowd_table(games), "crowd_contrast.csv")
  write_table(context_table(primary), "context_effects.csv")
  write_table(long_raw_table(games), "long_raw_home_win.csv")
  validation <- validation_table(games)
  write_table(validation$summary, "validation.csv")
  write_table(validation$calibration, "calibration.csv")
  write_table(gap_table(games, coverage), "data_gaps.csv")
  if (any(games$league == "NFL" & is.finite(games$source_home_rest))) {
    write_table(nfl_rest_check(games), "nfl_rest_check.csv")
  }
  invisible(games)
}

crowd_table <- function(games) {
  specs <- list(
    list("NBA", "2017–19, fans", quote(league == "NBA" & season_start %in% 2017:2018 & site_class == "home" & crowd == "normal")),
    list("NBA", "2019–20 bubble (neutral)", quote(league == "NBA" & site_class == "bubble")),
    list("NBA", "2020–21 empty", quote(league == "NBA" & season_start == 2020 & site_class == "home" & crowd == "no_fans")),
    list("NBA", "2020–21 reduced", quote(league == "NBA" & season_start == 2020 & site_class == "home" & crowd == "reduced")),
    list("NBA", "2021–23, fans", quote(league == "NBA" & season_start %in% 2021:2022 & site_class == "home" & crowd == "normal")),
    list("NFL", "2018–19 seasons", quote(league == "NFL" & season_start %in% 2018:2019 & site_class == "home")),
    list("NFL", "2020 empty", quote(league == "NFL" & season_start == 2020 & site_class == "home" & crowd == "no_fans")),
    list("NFL", "2020 reduced", quote(league == "NFL" & season_start == 2020 & site_class == "home" & crowd == "reduced")),
    list("NFL", "2021–22 seasons", quote(league == "NFL" & season_start %in% 2021:2022 & site_class == "home")),
    list("MLB", "2018–19 seasons", quote(league == "MLB" & season_start %in% 2018:2019 & site_class == "home")),
    list("MLB", "2020 empty home", quote(league == "MLB" & season_start == 2020 & site_class == "home")),
    list("MLB", "2020 makeshift home", quote(league == "MLB" & season_start == 2020 & site_class == "makeshift")),
    list("MLB", "2021–22 seasons", quote(league == "MLB" & season_start %in% 2021:2022 & site_class == "home")),
    list("NHL", "2018–20 seasons", quote(league == "NHL" & season_start %in% 2018:2019 & site_class == "home")),
    list("NHL", "2020–21 restricted (attendance unknown)", quote(league == "NHL" & season_start == 2020 & site_class == "home")),
    list("NHL", "2021–23 seasons", quote(league == "NHL" & season_start %in% 2021:2022 & site_class == "home"))
  )
  rows <- lapply(specs, function(spec) {
    keep <- with(games, eval(spec[[3]]))
    sl <- games[which(keep & is.finite(games$leftover_margin)), , drop = FALSE]
    stats <- edge_pair(sl, paste(spec[[1]], spec[[2]], sep = "|"))
    data.frame(
      league = spec[[1]], group = spec[[2]], unit = unname(scoring_unit[[spec[[1]]]]),
      n = stats$n,
      leftover_margin = stats$leftover_margin, margin_lo = stats$margin_lo, margin_hi = stats$margin_hi,
      leftover_wp = stats$leftover_wp, wp_lo = stats$wp_lo, wp_hi = stats$wp_hi,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

context_table <- function(primary) {
  rows <- list()
  add <- function(df, league, family, level, flagged) {
    if (!nrow(df)) return()
    stats <- edge_pair(df, paste(league, family, level, sep = "|"))
    rows[[length(rows) + 1L]] <<- data.frame(
      league = league, family = family, level = level, flagged = flagged, unit = unname(scoring_unit[[league]]),
      n = stats$n,
      leftover_margin = stats$leftover_margin, margin_lo = stats$margin_lo, margin_hi = stats$margin_hi,
      leftover_wp = stats$leftover_wp, wp_lo = stats$wp_lo, wp_hi = stats$wp_hi,
      stringsAsFactors = FALSE
    )
  }
  for (lg in c("MLB", "NBA", "NFL", "NHL")) {
    sub <- primary[primary$league == lg, , drop = FALSE]
    pick <- function(cond) sub[which(cond), , drop = FALSE]
    add(pick(sub$home_rest_location %in% "home"), lg, "Rest", "Home team rested at home", FALSE)
    add(pick(sub$home_rest_location %in% "road"), lg, "Rest", "Home team rested on the road", FALSE)
    add(pick(sub$away_trip_game %in% 1), lg, "Road trip", "Away team, trip game 1", FALSE)
    add(pick(sub$away_trip_game %in% 2), lg, "Road trip", "Away team, trip game 2", FALSE)
    add(pick(sub$away_trip_game %in% 3), lg, "Road trip", "Away team, trip game 3", FALSE)
    add(pick(is.finite(sub$away_trip_game) & sub$away_trip_game >= 4), lg, "Road trip", "Away team, trip game 4+", FALSE)
    if (lg %in% c("NBA", "NHL")) {
      known <- is.finite(sub$home_days_rest) & is.finite(sub$away_days_rest)
      add(pick(known & !sub$home_b2b & !sub$away_b2b), lg, "Back-to-back", "Neither on a back-to-back", FALSE)
      add(pick(known & sub$away_b2b %in% TRUE & !sub$home_b2b %in% TRUE), lg, "Back-to-back", "Away team on a back-to-back", FALSE)
      add(pick(known & sub$home_b2b %in% TRUE & !sub$away_b2b %in% TRUE), lg, "Back-to-back", "Home team on a back-to-back", FALSE)
      add(pick(known & sub$home_b2b %in% TRUE & sub$away_b2b %in% TRUE), lg, "Back-to-back", "Both on a back-to-back", FALSE)
    }
    miles <- sub$away_miles
    add(sub[is.finite(miles) & miles < 500, ], lg, "Miles", "Away team traveled under 500 miles", FALSE)
    add(sub[is.finite(miles) & miles >= 500 & miles < 1000, ], lg, "Miles", "Away team traveled 500–1,000 miles", FALSE)
    add(sub[is.finite(miles) & miles >= 1000, ], lg, "Miles", "Away team traveled over 1,000 miles", FALSE)
    add(sub[!is.na(sub$away_tz_cross) & abs(sub$away_tz_cross) < 1, ], lg, "Time zone", "Away team, same time zone", FALSE)
    add(sub[!is.na(sub$away_tz_cross) & sub$away_tz_cross <= -1, ], lg, "Time zone", "Away team, westward", FALSE)
    add(sub[sub$away_eastward %in% TRUE, ], lg, "Time zone", "Away team, eastward", TRUE)
    add(sub[sub$denver_utah %in% TRUE, ], lg, "Altitude", "Denver or Utah venue", TRUE)
    add(sub[sub$denver_utah %in% FALSE, ], lg, "Altitude", "Other venues", FALSE)
  }
  do.call(rbind, rows)
}

long_raw_table <- function(games) {
  use <- games$site_class == "home" & games$in_long_raw
  sub <- games[use, , drop = FALSE]
  rows <- lapply(split(sub, paste(sub$league, sub$season_start)), function(df) {
    stats <- boot_mean(df$home_win, df$home_franchise, paste("long", df$league[1], df$season_start[1], sep = "|"))
    data.frame(
      league = df$league[1],
      season_start = df$season_start[1],
      season_label = season_label(df$league[1], df$season_start[1]),
      season_complete = df$season_complete[1],
      n = stats$n,
      raw_home_win = stats$estimate,
      lo = stats$lo,
      hi = stats$hi,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$league, out$season_start), ]
}

validation_table <- function(games) {
  summaries <- list()
  calibrations <- list()
  for (lg in c("MLB", "NBA", "NFL", "NHL")) {
    sub <- games[games$league == lg & games$season_complete & games$season_start >= report_start_year, , drop = FALSE]
    sub <- sub[order(sub$game_date, sub$game_id), ]
    decisive <- sub$home_win %in% c(0, 1) & is.finite(sub$p_home)
    y <- sub$home_win[decisive]
    p_model <- sub$p_home[decisive]
    p_naive <- naive_home_prob(sub)[decisive]
    p_coin <- rep(0.5, sum(decisive))
    logloss <- function(p) {
      p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
      -mean(y * log(p) + (1 - y) * log(1 - p))
    }
    brier <- function(p) mean((p - y)^2)
    bin <- cut(p_model, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
    cal <- lapply(split(seq_along(y), bin), function(ii) {
      if (!length(ii)) return(NULL)
      data.frame(
        league = lg,
        bin = as.character(bin[ii[1]]),
        n = length(ii),
        mean_p = mean(p_model[ii]),
        mean_y = mean(y[ii]),
        stringsAsFactors = FALSE
      )
    })
    cal <- do.call(rbind, cal)
    ece <- sum(cal$n / sum(cal$n) * abs(cal$mean_y - cal$mean_p))
    played <- is.finite(sub$expected_margin) & is.finite(sub$margin)
    summaries[[lg]] <- data.frame(
      league = lg,
      n = sum(decisive),
      n_ties = sum(sub$home_win == 0.5, na.rm = TRUE),
      logloss_model = logloss(p_model),
      logloss_naive_home = logloss(p_naive),
      logloss_coin = logloss(p_coin),
      brier_model = brier(p_model),
      brier_naive_home = brier(p_naive),
      brier_coin = brier(p_coin),
      ece = ece,
      margin_cor = stats::cor(sub$expected_margin[played], sub$margin[played]),
      margin_rmse = sqrt(mean((sub$expected_margin[played] - sub$margin[played])^2)),
      stringsAsFactors = FALSE
    )
    calibrations[[lg]] <- cal
  }
  list(summary = do.call(rbind, summaries), calibration = do.call(rbind, calibrations))
}

naive_home_prob <- function(sub) {
  home <- sub$site_class == "home"
  rate <- rep(0.5, nrow(sub))
  if (!any(home)) return(rate)
  day <- as.character(sub$game_date)
  home_days <- sort(unique(day[home]))
  sums <- tapply(sub$home_win[home], day[home], sum)[home_days]
  ns <- tapply(sub$home_win[home], day[home], length)[home_days]
  before_sum <- c(0, cumsum(sums)[-length(sums)])
  before_n <- c(0, cumsum(ns)[-length(ns)])
  day_rate <- ifelse(before_n > 0, before_sum / before_n, 0.5)
  names(day_rate) <- home_days
  rate[home] <- unname(day_rate[day[home]])
  rate
}

nfl_rest_check <- function(games) {
  use <- games$league == "NFL" & games$week > 1 & is.finite(games$home_days_rest) & is.finite(games$source_home_rest)
  diff <- games$home_days_rest[use] - games$source_home_rest[use]
  data.frame(
    n = sum(use),
    mean_ours_minus_nflverse = mean(diff),
    share_within_1_day = mean(abs(diff) <= 1),
    note = "Ours is calendar days between games, minus 1. The first game of a season is missing. nflverse week-1 rest is a placeholder and is excluded.",
    stringsAsFactors = FALSE
  )
}

gap_table <- function(games, coverage) {
  coord_miss <- 1 - sum(coverage$n_with_coords) / sum(coverage$n_games)
  rows <- data.frame(
    item = c(
      "NHL attendance",
      "NFL attendance",
      "MLB attendance zeros",
      "MLB seasons after the Retrosheet logs",
      "Coordinates",
      "Partial seasons",
      "Overtime and shootouts",
      "Within-metro travel",
      "Pregame ratings"
    ),
    detail = c(
      "The NHL schedule feed has no game attendance. 2020-21 is labeled restricted from the season calendar, not from turnstile counts.",
      "nflverse schedules have no attendance. 2020 empty versus reduced games follow the spectator policies compiled on the Wikipedia 2020 NFL season page, retrieved 2026-11-14.",
      "Outside 2020, a Retrosheet attendance of 0 is treated as unknown. In 2020 it is treated as an empty ballpark.",
      "Seasons not yet in the Retrosheet game logs, if any, come from baseballr::mlb_schedule and have no attendance.",
      sprintf("Share of games without coordinates: %.1f%%. Travel rows that need those games are dropped.", 100 * coord_miss),
      "NFL 2026 and the 2026-27 NBA and NHL seasons were in progress on 2026-11-14. They stay in the data and are open points on chart A. MLB 2026 regular season is treated as complete.",
      "Margins use the published final score, including overtime. An NHL shootout win is whatever one-goal margin the NHL feed records.",
      "Usual-home coordinates are city or arena-neighborhood points. Trips under about 50 miles are local travel, not surveyed stadium-to-stadium routes.",
      "Intervals treat the pregame ratings as fixed. They do not include uncertainty from the rating model itself. Ratings for games on the same date ignore one another."
    ),
    stringsAsFactors = FALSE
  )
  rows
}
