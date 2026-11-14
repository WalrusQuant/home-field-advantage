fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)
fmt_num <- function(x, digits = 2) sprintf(paste0("%.", digits, "f"), x)
fmt_pm <- function(est, lo, hi, digits = 2, scale = 1) {
  sprintf(
    paste0("%.", digits, "f", " (%.", digits, "f to %.", digits, "f)"),
    scale * est, scale * lo, scale * hi
  )
}

direction <- function(delta) {
  if (!is.finite(delta) || abs(delta) < 1e-8) "flat"
  else if (delta > 0) "up"
  else "down"
}

write_results <- function() {
  season <- read_table("season_home_edge.csv")
  tiers <- read_table("tier_home_edge.csv")
  teams <- read_table("team_season.csv")
  valid <- read_table("validation.csv")
  coverage <- read_table("coverage.csv")
  gaps <- read_table("data_gaps.csv")
  crowd <- read_table("crowd_contrast.csv")

  lines <- c(
    "Numbers below are read from `outputs/tables/` after the pipeline run. Intervals are 95% cluster-bootstrap intervals by home team.",
    ""
  )
  lines <- c(lines, "### Coverage", "")
  for (lg in c("MLB", "NBA", "NFL", "NHL")) {
    sub <- coverage[coverage$league == lg & coverage$season_start >= report_start_year, ]
    home_n <- sum(sub$n_home_edge)
    game_n <- sum(sub$n_games)
    done <- sub[sub$season_complete, ]
    partial <- sub[!sub$season_complete, ]
    lines <- c(lines, sprintf(
      "- **%s**: %s games in seasons starting %d–%s (%s of them in the true-home sample). Completed through %s.%s",
      lg,
      format(game_n, big.mark = ","),
      min(sub$season_start),
      max(sub$season_label),
      format(home_n, big.mark = ","),
      if (nrow(done)) max(done$season_label) else "none",
      if (nrow(partial)) paste0(" Partial: ", paste(partial$season_label, collapse = ", "), ".") else ""
    ))
  }
  lines <- c(lines, "", "### Model validation", "",
             "Log loss is on decisive games in completed seasons from 2010 on, using the pregame win probability that includes the home term. Lower is better. The coin-flip baseline is 0.5. The naive home baseline is the league's home win rate before that date.")
  for (i in seq_len(nrow(valid))) {
    row <- valid[i, ]
    lines <- c(lines, sprintf(
      "- **%s** (%s games, %s ties left out of the scores): model log loss %s, naive home %s, coin flip %s. Brier %s vs %s vs %s. Calibration error %s. Correlation of expected and actual margin %s, RMSE %s %s.",
      row$league, format(row$n, big.mark = ","), format(row$n_ties, big.mark = ","),
      fmt_num(row$logloss_model, 3), fmt_num(row$logloss_naive_home, 3), fmt_num(row$logloss_coin, 3),
      fmt_num(row$brier_model, 3), fmt_num(row$brier_naive_home, 3), fmt_num(row$brier_coin, 3),
      fmt_num(row$ece, 3), fmt_num(row$margin_cor, 2), fmt_num(row$margin_rmse, 2),
      unname(scoring_unit[[row$league]])
    ))
  }

  lines <- c(lines, "", "### Chart A, early window versus the latest completed season", "")
  for (lg in c("NBA", "NFL", "MLB", "NHL")) {
    sub <- season[season$league == lg & season$season_complete, ]
    early <- sub[sub$season_start <= 2014, ]
    late <- sub[sub$season_start == max(sub$season_start), ]
    unit <- late$unit[1]
    d_raw <- mean(late$raw_home_win) - mean(early$raw_home_win)
    d_m <- mean(late$leftover_margin) - mean(early$leftover_margin)
    d_w <- mean(late$leftover_wp) - mean(early$leftover_wp)
    lines <- c(lines, sprintf(
      "- **%s** %s: raw home win rate %s, leftover margin %s %s, win-probability edge %s percentage points, team-edge IQR %s %s. From the 2010–14 mean to %s, raw went %s (%+.1f percentage points), leftover margin went %s (%+.2f), and the win-probability edge went %s (%+.1f percentage points).",
      lg, late$season_label[1],
      fmt_pm(late$raw_home_win, late$raw_lo, late$raw_hi, 1, 100),
      fmt_pm(late$leftover_margin, late$margin_lo, late$margin_hi, 2), unit,
      fmt_pm(late$leftover_wp, late$wp_lo, late$wp_hi, 1, 100),
      fmt_pm(late$team_iqr, late$iqr_lo, late$iqr_hi, 2), unit,
      late$season_label[1],
      direction(d_raw), 100 * d_raw,
      direction(d_m), d_m,
      direction(d_w), 100 * d_w
    ))
  }

  lines <- c(lines, "", "### Chart B, home edge by pregame gap", "",
             "Coin-flip is a neutral gap under 0.25 residual SD, modest is 0.25 to 0.75, and big is 0.75 or more. Completed true-home games.")
  for (lg in c("NBA", "NFL", "MLB", "NHL")) {
    sub <- tiers[tiers$league == lg, ]
    bits <- vapply(seq_len(nrow(sub)), function(i) {
      sprintf(
        "%s margin %s, win-probability edge %s pp (n=%s)",
        sub$tier[i],
        fmt_pm(sub$leftover_margin[i], sub$margin_lo[i], sub$margin_hi[i], 2),
        fmt_pm(sub$leftover_wp[i], sub$wp_lo[i], sub$wp_hi[i], 1, 100),
        format(sub$n[i], big.mark = ",")
      )
    }, character(1))
    lines <- c(lines, paste0("- **", lg, "** (", sub$unit[1], "): ", paste(bits, collapse = "; "), "."))
  }

  lines <- c(lines, "", "### Chart C", "")
  use <- teams[teams$season_complete %in% TRUE, ]
  for (lg in c("NBA", "NFL", "MLB", "NHL")) {
    sl <- use[use$league == lg, ]
    r <- stats::cor(sl$raw_home_win, sl$leftover_shrunk, use = "complete.obs")
    lines <- c(lines, sprintf(
      "- **%s**: correlation of raw home win rate and shrunk leftover margin across team-seasons is %s (n=%s).",
      lg, fmt_num(r, 2), format(nrow(sl), big.mark = ",")
    ))
  }

  lines <- c(lines, "", "### 2020 contrast, leftover margin", "")
  for (i in seq_len(nrow(crowd))) {
    row <- crowd[i, ]
    if (!is.finite(row$n) || row$n == 0) next
    lines <- c(lines, sprintf(
      "- **%s, %s** (n=%s): %s %s.",
      row$league, row$group, format(row$n, big.mark = ","),
      fmt_pm(row$leftover_margin, row$margin_lo, row$margin_hi, 2), row$unit
    ))
  }

  lines <- c(lines, "", "### Data gaps", "")
  for (i in seq_len(nrow(gaps))) {
    lines <- c(lines, sprintf("- **%s.** %s", gaps$item[i], gaps$detail[i]))
  }

  chart_dir <- path_in("outputs", "charts")
  charts <- sort(list.files(chart_dir, pattern = "\\.png$"))
  lines <- c(lines, "", "### Chart files", "")
  for (chart in charts) lines <- c(lines, paste0("- `outputs/charts/", chart, "`"))
  lines <- c(lines, "")

  text <- paste(lines, collapse = "\n")
  writeLines(text, path_in("outputs", "RESULTS.md"))
  splice_readme(text)
  say("Wrote outputs/RESULTS.md and updated the README results block")
  invisible(text)
}

splice_readme <- function(text) {
  path <- path_in("README.md")
  readme <- readLines(path, warn = FALSE)
  start <- grep("<!-- RESULTS:START -->", readme, fixed = TRUE)
  end <- grep("<!-- RESULTS:END -->", readme, fixed = TRUE)
  if (length(start) != 1L || length(end) != 1L || end <= start) {
    stop("README is missing the RESULTS markers.")
  }
  body <- strsplit(text, "\n", fixed = TRUE)[[1]]
  updated <- c(readme[seq_len(start)], body, readme[end:length(readme)])
  writeLines(updated, path)
}
