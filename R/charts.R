theme_home <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3, color = "#1A1A1A"),
      plot.subtitle = ggplot2::element_text(color = "#333333", size = base_size - 1, margin = ggplot2::margin(b = 8)),
      plot.caption = ggplot2::element_text(color = "#4A4A4A", size = base_size - 4, hjust = 0, lineheight = 1.15),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1, color = "#1A1A1A"),
      strip.background = ggplot2::element_rect(fill = "#EFECE3", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E4E1D8"),
      axis.title = ggplot2::element_text(color = "#1A1A1A"),
      axis.text = ggplot2::element_text(color = "#1A1A1A"),
      legend.position = "bottom",
      plot.background = ggplot2::element_rect(fill = "#FAFAF7", color = NA),
      panel.background = ggplot2::element_rect(fill = "#FAFAF7", color = NA),
      plot.margin = ggplot2::margin(12, 16, 12, 12)
    )
}

save_plot <- function(plot, filename, width, height) {
  dest <- path_in("outputs", "charts", filename)
  ensure_dir(dirname(dest))
  ggplot2::ggsave(dest, plot, width = width, height = height, dpi = 160, bg = "#FAFAF7")
  say("Wrote ", dest)
  dest
}

read_table <- function(name) {
  readr::read_csv(path_in("outputs", "tables", name), show_col_types = FALSE)
}

measure_long <- function(season) {
  specs <- list(
    c("Raw home win %", "raw_home_win", "raw_lo", "raw_hi", "100"),
    c("Leftover margin", "leftover_margin", "margin_lo", "margin_hi", "1"),
    c("Leftover win probability", "leftover_wp", "wp_lo", "wp_hi", "100"),
    c("Spread of team home edges", "team_iqr", "iqr_lo", "iqr_hi", "1")
  )
  pieces <- lapply(specs, function(spec) {
    scale <- as.numeric(spec[5])
    data.frame(
      league = season$league,
      season_start = season$season_start,
      season_label = season$season_label,
      season_complete = season$season_complete,
      unit = season$unit,
      measure = spec[1],
      estimate = season[[spec[2]]] * scale,
      lo = season[[spec[3]]] * scale,
      hi = season[[spec[4]]] * scale,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  out$measure <- factor(out$measure, levels = vapply(specs, function(s) s[1], character(1)))
  out$league_label <- paste0(out$league, " (", out$unit, ")")
  out
}

chart_a <- function(season, leagues = NULL, filename = "A_home_edge_over_time.png", width = 14, height = 12) {
  data <- measure_long(season)
  if (!is.null(leagues)) data <- data[data$league %in% leagues, , drop = FALSE]
  data$league_label <- factor(data$league_label, levels = unique(data$league_label[order(data$league)]))
  plot <- ggplot2::ggplot(data, ggplot2::aes(season_start, estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#1B4F72", alpha = 0.18) +
    ggplot2::geom_line(color = "#1B4F72", linewidth = 0.7) +
    ggplot2::geom_point(
      ggplot2::aes(shape = season_complete),
      color = "#1B4F72",
      fill = "#FAFAF7",
      size = 2.1
    ) +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
    ggplot2::facet_grid(measure ~ league_label, scales = "free_y") +
    ggplot2::labs(
      title = "Home edge over time, four ways",
      subtitle = "Same true home games. Ribbons are 95% cluster-bootstrap intervals. Open points are partial seasons.",
      x = "Season start year",
      y = NULL,
      caption = "Win rate and win-probability edge are in percentage points. Leftover margin removes pregame team strength. Spread is the IQR of shrunk team home edges. Neutral, bubble, makeshift, and off-campus games are excluded."
    ) +
    theme_home(13)
  save_plot(plot, filename, width, height)
}

chart_a_index <- function(season) {
  data <- measure_long(season)
  data <- data[data$season_complete, , drop = FALSE]
  bases <- aggregate(estimate ~ league + measure, data = data[data$season_start <= 2014, ], FUN = mean)
  names(bases)[3] <- "base"
  data <- merge(data, bases, by = c("league", "measure"), all.x = TRUE)
  data <- data[is.finite(data$base) & data$base > 0, ]
  data$index <- data$estimate / data$base
  plot <- ggplot2::ggplot(data, ggplot2::aes(season_start, index, color = measure)) +
    ggplot2::geom_hline(yintercept = 1, color = "#B0ADA4", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~ league, nrow = 1) +
    ggplot2::scale_color_manual(values = c(
      "Raw home win %" = "#0072B2",
      "Leftover margin" = "#D55E00",
      "Leftover win probability" = "#009E73",
      "Spread of team home edges" = "#CC79A7"
    )) +
    ggplot2::labs(
      title = "Do the home-edge measures move together?",
      subtitle = "Each series is divided by its 2010–14 mean. 1.0 is that early-window level. Completed seasons only.",
      x = "Season start year",
      y = "Index (2010–14 mean = 1)",
      color = NULL,
      caption = "Intervals for the unscaled series are on chart A. A split here means the raw win rate and the strength-adjusted edge are not telling the same story."
    ) +
    theme_home(14)
  save_plot(plot, "A_measures_indexed.png", 13, 6.2)
}

chart_long <- function(long) {
  long$pct <- 100 * long$raw_home_win
  long$lo <- 100 * long$lo
  long$hi <- 100 * long$hi
  plot <- ggplot2::ggplot(long, ggplot2::aes(season_start, pct)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "#1B4F72", alpha = 0.18) +
    ggplot2::geom_line(color = "#1B4F72", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 2010, linetype = "dashed", color = "#9A3412") +
    ggplot2::facet_wrap(~ league, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = "Raw home win rate, further back",
      subtitle = "NFL from 1999 (nflverse) and MLB from 1970 (Retrosheet game logs). The dashed line is where the main study starts.",
      x = "Season",
      y = "Home win rate (%)",
      caption = "True home games only. The Retrosheet notice in the README applies to the MLB series. This chart is the raw rate, with no team-strength adjustment."
    ) +
    theme_home(14)
  save_plot(plot, "A_long_raw_home_win.png", 11, 7.5)
}

chart_b <- function(tiers) {
  margin <- data.frame(
    league = tiers$league, tier = tiers$tier, unit = tiers$unit, n = tiers$n,
    scale = "Leftover margin", estimate = tiers$leftover_margin, lo = tiers$margin_lo, hi = tiers$margin_hi
  )
  wp <- data.frame(
    league = tiers$league, tier = tiers$tier, unit = tiers$unit, n = tiers$n,
    scale = "Win-probability edge (percentage points)",
    estimate = 100 * tiers$leftover_wp, lo = 100 * tiers$wp_lo, hi = 100 * tiers$wp_hi
  )
  data <- rbind(margin, wp)
  data <- data[data$n >= 200, , drop = FALSE]
  data$tier <- factor(data$tier, levels = c("Coin-flip", "Modest gap", "Big gap"))
  data$league_label <- paste0(data$league, " (", data$unit, ")")
  leagues <- unique(data$league_label[order(data$league)])
  data$scale_short <- ifelse(grepl("^Win", data$scale), "Win prob. (pp)", "Margin")
  data$panel <- factor(
    paste(data$league_label, data$scale_short, sep = "\n"),
    levels = c(
      paste(leagues, "Margin", sep = "\n"),
      paste(leagues, "Win prob. (pp)", sep = "\n")
    )
  )
  data$label <- sprintf("%.2f\nn=%s", data$estimate, format(data$n, big.mark = ","))
  plot <- ggplot2::ggplot(data, ggplot2::aes(tier, estimate, group = 1)) +
    ggplot2::geom_hline(yintercept = 0, color = "#B0ADA4", linewidth = 0.4) +
    ggplot2::geom_line(color = "#1B4F72", linewidth = 0.6) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lo, ymax = hi), color = "#1B4F72", size = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.8, size = 2.6, lineheight = 0.9, color = "#1A1A1A") +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 4) +
    ggplot2::labs(
      title = "Is the points edge flat when the win-rate edge is not?",
      subtitle = "Tiers are the pregame neutral gap: under 0.25 residual SD, 0.25 to 0.75, and 0.75 or more. Completed seasons, true home games.",
      x = "Pregame strength gap",
      y = NULL,
      caption = "Margin removes pregame team strength. Win prob. is the result minus the neutral win probability, in percentage points. 95% intervals. Tiers under 200 games stay in the table only."
    ) +
    theme_home(13) +
    ggplot2::theme(plot.margin = ggplot2::margin(12, 16, 12, 12))
  save_plot(plot, "B_home_edge_by_strength_tier.png", 13.5, 8.2)
}

chart_c <- function(teams) {
  data <- teams[teams$season_complete %in% TRUE, ]
  data$label <- ""
  for (lg in unique(data$league)) {
    idx <- which(data$league == lg)
    ord <- idx[order(data$raw_home_win[idx])]
    pick <- unique(c(utils::head(ord, 3), utils::tail(ord, 3)))
    data$label[pick] <- data$franchise[pick]
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(raw_home_win, leftover_shrunk, color = mean_home_strength)) +
    ggplot2::geom_point(alpha = 0.8, size = 1.8) +
    ggrepel::geom_text_repel(ggplot2::aes(label = label), size = 2.6, color = "#1A1A1A", max.overlaps = 20, seed = 1) +
    ggplot2::facet_wrap(~ league, scales = "free", ncol = 2) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_color_gradient(low = "#4C78A8", high = "#E4572E") +
    ggplot2::labs(
      title = "A weak team's home win rate is not its home edge",
      subtitle = "Each point is a team-season. Vertical position is the home residual after pregame team strength, shrunk toward the league.",
      x = "Raw home win rate",
      y = "Shrunk leftover margin",
      color = "Pregame\nstrength",
      caption = "Color is the team's average pregame strength in its home games. Completed seasons. A low raw home win rate with an ordinary leftover margin is the raw number being pulled around by team quality."
    ) +
    theme_home(14)
  save_plot(plot, "C_raw_vs_leftover_team_season.png", 12, 9)
}

forest <- function(data, title, subtitle, caption, filename, width, height) {
  data <- data[data$n >= 30, , drop = FALSE]
  data$estimate <- data$leftover_margin
  data$level <- factor(data$level, levels = rev(unique(data$level)))
  data$league_label <- paste0(data$league, " (", data$unit, ")")
  plot <- ggplot2::ggplot(data, ggplot2::aes(estimate, level)) +
    ggplot2::geom_vline(xintercept = 0, color = "#B0ADA4", linewidth = 0.4) +
    ggplot2::geom_pointrange(
      ggplot2::aes(xmin = margin_lo, xmax = margin_hi, shape = flagged),
      color = "#1B4F72",
      size = 0.55
    ) +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
    ggplot2::facet_wrap(~ league_label, scales = "free", ncol = 2) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Leftover margin (95% interval)", y = NULL, caption = caption) +
    theme_home(13)
  save_plot(plot, filename, width, height)
}

chart_d <- function(crowd) {
  crowd$flagged <- grepl("empty|reduced|bubble|makeshift|restricted", crowd$group, ignore.case = TRUE)
  crowd$level <- crowd$group
  forest(
    crowd,
    title = "2020 and the buildings around it",
    subtitle = "Leftover margin after pregame team strength. Bubbles and makeshift homes are separate from empty or reduced home buildings.",
    caption = "NBA and MLB use attendance. NFL 2020 uses compiled spectator policies; nearby NFL seasons are not attendance-checked. NHL 2020-21 has no attendance in the feed. Groups under 30 games are omitted.",
    filename = "D_2020_crowd.png",
    width = 12.5,
    height = 8.5
  )
}

chart_e <- function(context) {
  rest <- context[context$family %in% c("Rest", "Road trip", "Back-to-back"), ]
  travel <- context[context$family %in% c("Miles", "Time zone", "Altitude"), ]
  forest(
    rest,
    title = "Rest, road trips, and back-to-backs",
    subtitle = "Leftover margin on true home games in completed seasons, after pregame team strength.",
    caption = "Rest location is the site of that team's previous game. A back-to-back is a game the day after another game. Back-to-backs are shown for NBA and NHL. Groups with fewer than 30 games are omitted.",
    filename = "E_rest_trip_back_to_back.png",
    width = 12.5,
    height = 8.8
  )
  forest(
    travel,
    title = "Miles, time zones, and altitude",
    subtitle = "The traveling team is the away team. Eastward trips and Denver or Utah venues are marked with diamonds.",
    caption = "Miles are great-circle distance from the previous venue. Eastward means the clock moves forward by at least one hour. Altitude is a Denver or Utah venue, not every high city. Groups with fewer than 30 games are omitted.",
    filename = "E_travel_altitude.png",
    width = 12.5,
    height = 8.2
  )
}

chart_calibration <- function(cal) {
  plot <- ggplot2::ggplot(cal, ggplot2::aes(mean_p, mean_y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "#B0ADA4", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = n), color = "#1B4F72", alpha = 0.85) +
    ggplot2::facet_wrap(~ league, nrow = 1) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::scale_size_area(max_size = 5) +
    ggplot2::labs(
      title = "Are the pregame win probabilities calibrated?",
      subtitle = "Deciles of the model's win probability, including the home term, against the observed home-win rate.",
      x = "Predicted home win probability",
      y = "Observed home win rate",
      size = "Games",
      caption = "Completed seasons from 2010 on. Ties are left out. The line is perfect calibration. This is a check on the ratings, not a betting result."
    ) +
    theme_home(13)
  save_plot(plot, "V_calibration.png", 13, 5.2)
}

run_charts <- function() {
  season <- read_table("season_home_edge.csv")
  chart_a(season)
  chart_a(season[season$league %in% c("NBA", "NFL"), ], NULL, "A_home_edge_nba_nfl.png", 12, 11)
  chart_a_index(season)
  chart_long(read_table("long_raw_home_win.csv"))
  chart_b(read_table("tier_home_edge.csv"))
  chart_c(read_table("team_season.csv"))
  chart_d(read_table("crowd_contrast.csv"))
  chart_e(read_table("context_effects.csv"))
  chart_calibration(read_table("calibration.csv"))
}
