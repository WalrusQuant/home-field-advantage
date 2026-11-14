test_that("a planted home edge is recovered and later games do not leak", {
  set.seed(11)
  n_teams <- 8L
  strength <- seq(-8, 8, length.out = n_teams)
  n <- 500L
  home <- sample.int(n_teams, n, replace = TRUE)
  away <- sample.int(n_teams, n, replace = TRUE)
  same <- home == away
  away[same] <- (away[same] %% n_teams) + 1L
  dates <- as.Date("2018-01-01") + sample.int(160, n, replace = TRUE)
  neutral <- rep(0L, n)
  neutral[seq_len(40)] <- 1L
  margin <- strength[home] - strength[away] + ifelse(neutral == 1L, 0, 3) + rnorm(n, 0, 8)
  games <- data.frame(game_date = dates, home_ix = home, away_ix = away, margin = margin, neutral = neutral)
  pred <- predict_pregame(
    games, half_life_days = 400, n_teams = n_teams,
    lambda_team = 1, lambda_home = 2, home_prior = 3, sigma_init = 8, sigma_floor = 1
  )
  late <- dates >= as.Date("2018-05-01")
  expect_gt(mean(pred$h_pregame[late]), 2)
  expect_lt(mean(pred$h_pregame[late]), 4)
  expect_gt(cor(pred$home_strength[late], strength[home[late]]), 0.8)

  present <- sort(unique(dates))
  cut_date <- present[max(2L, length(present) %/% 2L)]
  on_day <- dates == cut_date
  earlier <- games[dates <= cut_date, , drop = FALSE]
  pred_earlier <- predict_pregame(
    earlier, half_life_days = 400, n_teams = n_teams,
    lambda_team = 1, lambda_home = 2, home_prior = 3, sigma_init = 8, sigma_floor = 1
  )
  expect_equal(
    pred$neutral_margin[on_day],
    pred_earlier$neutral_margin[earlier$game_date == cut_date],
    tolerance = 1e-8
  )
})

test_that("neutral games do not receive the home term", {
  games <- data.frame(
    game_date = as.Date("2020-01-01") + c(0, 1, 2, 3),
    home_ix = c(1L, 1L, 2L, 2L),
    away_ix = c(2L, 2L, 1L, 1L),
    margin = c(5, 5, 1, 1),
    neutral = c(0L, 0L, 0L, 1L)
  )
  pred <- predict_pregame(
    games, half_life_days = 1000, n_teams = 2,
    lambda_team = 0.1, lambda_home = 0.1, home_prior = 0, sigma_init = 5, sigma_floor = 1
  )
  expect_equal(pred$expected_margin[4], pred$neutral_margin[4])
  expect_gt(pred$expected_margin[3], pred$neutral_margin[3])
})

test_that("tier cutpoints put a coin-flip below 0.25 SD and a big gap at 0.75", {
  tiers <- cut(
    c(0, 0.24, 0.25, 0.74, 0.75, 2),
    tier_breaks, tier_labels, right = FALSE, include.lowest = TRUE
  )
  expect_equal(as.character(tiers), c("Coin-flip", "Coin-flip", "Modest gap", "Modest gap", "Big gap", "Big gap"))
})
