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

# Independent oracle: construct X and refit only strictly earlier observations.
direct_pregame <- function(games, half_life_days, n_teams,
                           lambda_team = 4, lambda_home = 10, home_prior = 2,
                           sigma_init = 12, sigma_floor = 1) {
  n <- nrow(games)
  x <- matrix(0, n, n_teams + 1L)
  x[cbind(seq_len(n), games$home_ix)] <- 1
  x[cbind(seq_len(n), games$away_ix)] <- -1
  x[, n_teams + 1L] <- 1 - games$neutral
  outcome_date <- if ("outcome_date" %in% names(games)) games$outcome_date else games$game_date
  result <- lapply(seq_len(n), function(i) {
    past <- which(outcome_date < games$game_date[i])
    w <- 2 ^ (-as.numeric(games$game_date[i] - games$game_date[past]) / half_life_days)
    xp <- x[past, , drop = FALSE]
    y <- games$margin[past]
    beta <- solve(
      crossprod(xp, w * xp) + diag(c(rep(lambda_team, n_teams), lambda_home)),
      crossprod(xp, w * y) + c(rep(0, n_teams), lambda_home * home_prior)
    )
    sigma <- if (sum(w) >= 20) {
      max(sigma_floor, sqrt(sum(w * (y - xp %*% beta)^2) / sum(w)))
    } else sigma_init
    nm <- beta[games$home_ix[i]] - beta[games$away_ix[i]]
    em <- sum(x[i, ] * beta)
    data.frame(neutral_margin = nm, expected_margin = em,
               p_neutral = pnorm(nm / sigma), p_home = pnorm(em / sigma),
               h_pregame = beta[n_teams + 1L], sigma_pregame = sigma,
               home_strength = beta[games$home_ix[i]],
               away_strength = beta[games$away_ix[i]])
  })
  do.call(rbind, result)
}

test_that("pregame outputs match direct weighted ridge with unsorted dates", {
  set.seed(31)
  n <- 180L
  home <- sample.int(5L, n, TRUE)
  games <- data.frame(
    game_date = as.Date("2020-01-01") + sample(0:60, n, TRUE),
    home_ix = home,
    away_ix = (home + sample.int(4L, n, TRUE) - 1L) %% 5L + 1L,
    margin = rnorm(n, 3, 9), neutral = sample(0:1, n, TRUE)
  )
  pred <- predict_pregame(games, 15, 5, home_prior = 2)
  expect_equal(pred, direct_pregame(games, 15, 5), tolerance = 1e-10)
  expect_true(any(pred$sigma_pregame != 12))
  expect_equal(predict_pregame(games, 15, 5, home_prior = 2, sigma_floor = 100),
               direct_pregame(games, 15, 5, sigma_floor = 100), tolerance = 1e-10)

  cutoff <- sort(unique(games$game_date))[30]
  changed <- games
  changed$margin[changed$game_date >= cutoff] <- 1e6
  changed_pred <- predict_pregame(changed, 15, 5, home_prior = 2)
  before <- games$game_date <= cutoff
  expect_equal(pred[before, ], changed_pred[before, ], tolerance = 1e-10)
  truncated <- predict_pregame(games[before, ], 15, 5, home_prior = 2)
  expect_equal(unname(as.matrix(pred[before, ])), unname(as.matrix(truncated)),
               tolerance = 1e-10)
  perm <- sample.int(n)
  reordered <- predict_pregame(games[perm, ], 15, 5, home_prior = 2)
  expect_equal(unname(as.matrix(pred)), unname(as.matrix(reordered[order(perm), ])),
               tolerance = 1e-10)
})

test_that("long spans and short half-lives retain finite correct predictions", {
  games <- data.frame(
    game_date = as.Date("2020-01-01") + c(0, 1024, 1025, 4096, 4097),
    home_ix = 1L, away_ix = 2L, margin = c(3, 5, 0, 7, 0), neutral = 0L
  )
  pred <- predict_pregame(games, 1, 2, home_prior = 2)
  expect_true(all(is.finite(as.matrix(pred))))
  expect_equal(pred, direct_pregame(games, 1, 2), tolerance = 1e-10)
})

test_that("date and neutral inputs cannot silently change model semantics", {
  games <- data.frame(game_date = as.Date("2020-01-01") + 0:1,
                      home_ix = 1L, away_ix = 2L, margin = c(3, 5), neutral = 0L)
  bad <- games
  bad$game_date <- as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + c(0, 3600)
  expect_error(predict_pregame(bad, 150, 2), "finite Date")
  bad$game_date <- c(as.Date(NA), games$game_date[2])
  expect_error(predict_pregame(bad, 150, 2), "finite Date")
  for (value in c(2, -1, NA)) {
    bad <- games
    bad$neutral[1] <- value
    expect_error(predict_pregame(bad, 150, 2), "only 0 or 1")
  }
  for (value in c(0, -1, Inf, NA)) {
    expect_error(predict_pregame(games, value, 2), "finite and positive")
  }
})

test_that("joint home term handles unidentified and identified home coefficients", {
  home <- rep(c(1L, 2L), 5)
  away <- 3L - home
  margin <- c(5, -1, 4, 0, 7, -2, 3, 1, 6, -3)
  expect_true(is.na(joint_home_term(home, away, margin, 2, neutral = TRUE)))
  neutral <- rep(c(FALSE, FALSE, TRUE, FALSE, TRUE), 2)
  x <- cbind(ifelse(home == 1L, 1, -1), ifelse(home == 2L, 1, -1), !neutral)
  beta <- solve(crossprod(x) + diag(c(1, 1, 0)), crossprod(x, margin))
  expect_equal(joint_home_term(home, away, margin, 2, neutral), unname(beta[3]))
})


test_that("suspended outcomes enter only after completion and retain start-date age", {
  games <- data.frame(
    game_date = as.Date(c("2024-04-11", "2024-05-01", "2024-08-31", "2024-09-01", "2024-09-02")),
    outcome_date = as.Date(c("2024-08-31", "2024-05-02", "2024-08-31", "2024-09-01", "2025-01-01")),
    home_ix = 1L, away_ix = 2L, margin = c(3, -2, 1, 4, 9), neutral = 0L
  )
  pred <- predict_pregame(games, 120, 2, home_prior = 2)
  expect_equal(pred, direct_pregame(games, 120, 2), tolerance = 1e-10)
  changed <- games
  changed$margin[1] <- 100
  changed_pred <- predict_pregame(changed, 120, 2, home_prior = 2)
  before <- games$game_date <= as.Date("2024-08-31")
  expect_identical(pred[before, ], changed_pred[before, ])
  expect_true(all(abs(pred$expected_margin[!before] - changed_pred$expected_margin[!before]) > 0.1))
  expect_equal(changed_pred, direct_pregame(changed, 120, 2), tolerance = 1e-10)
  # No prediction occurs on May 2: completion-only events must still be used.
  changed <- games
  changed$margin[2] <- 100
  expect_gt(abs(predict_pregame(changed, 120, 2, home_prior = 2)$expected_margin[3] -
                  pred$expected_margin[3]), 0.1)
  # Outcomes beyond the last prediction (including an as-of cutoff) cannot leak.
  changed <- games
  changed$margin[5] <- 1e6
  expect_identical(predict_pregame(changed, 120, 2, home_prior = 2), pred)
  perm <- c(4, 2, 5, 1, 3)
  reordered <- predict_pregame(games[perm, ], 120, 2, home_prior = 2)
  expect_equal(unname(as.matrix(reordered[order(perm), ])), unname(as.matrix(pred)),
               tolerance = 1e-10)
})

test_that("outcome dates default to starts and reject invalid availability", {
  games <- data.frame(game_date = as.Date("2024-04-11") + 0:2,
                      home_ix = 1L, away_ix = 2L, margin = c(3, 1, 2), neutral = 0L)
  baseline <- predict_pregame(games, 120, 2)
  games$outcome_date <- games$game_date
  expect_identical(predict_pregame(games, 120, 2), baseline)
  games$outcome_date[1] <- games$game_date[1] - 1
  expect_error(predict_pregame(games, 120, 2), "on or after game_date")
  games$outcome_date[1] <- as.Date(NA)
  expect_error(predict_pregame(games, 120, 2), "outcome_date must contain finite Date")
  games$outcome_date <- as.POSIXct(games$game_date)
  expect_error(predict_pregame(games, 120, 2), "outcome_date must contain finite Date")
})

test_that("delayed outcomes match direct WLS including residual scale", {
  set.seed(72)
  n <- 240L
  games <- data.frame(game_date = as.Date("2024-04-01") + sample(0:60, n, TRUE),
                      home_ix = sample(1:2, n, TRUE), margin = rnorm(n, 2, 8),
                      neutral = sample(0:1, n, TRUE))
  games$away_ix <- 3L - games$home_ix
  games$outcome_date <- games$game_date + sample(0:20, n, TRUE)
  pred <- predict_pregame(games, 30, 2, home_prior = 2)
  expect_true(any(pred$sigma_pregame != 12))
  expect_equal(pred, direct_pregame(games, 30, 2), tolerance = 1e-10)
})

test_that("fit_games forwards outcome dates and preserves the start-date fallback", {
  games <- data.frame(
    game_date = as.Date(c("2024-04-11", "2024-05-01", "2024-09-01")),
    outcome_date = as.Date(c("2024-08-31", "2024-05-01", "2024-09-01")),
    league = "MLB", season_start = 2024L, home_franchise = "A", away_franchise = "B",
    home_score = c(5, 2, 4), away_score = c(1, 3, 2), site_class = "home"
  )
  games$margin <- games$home_score - games$away_score
  games$home_win <- as.numeric(games$margin > 0)
  model_input <- data.frame(game_date = games$game_date, outcome_date = games$outcome_date,
                            home_ix = 1L, away_ix = 2L, margin = games$margin, neutral = 0L)
  expected <- predict_pregame(model_input, half_life_days[["MLB"]], 2,
                              lambda_team, lambda_home, home_prior[["MLB"]],
                              sigma_init[["MLB"]], sigma_floor[["MLB"]])
  expect_equal(fit_games(games)[names(expected)], expected)
  games$outcome_date <- NULL
  fallback <- fit_games(games)
  games$outcome_date <- games$game_date
  expect_equal(fit_games(games)[names(expected)], fallback[names(expected)])
})
