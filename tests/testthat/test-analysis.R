source(file.path(root, "R/analyze.R"))

test_that("validation warms the baseline before restricting the scoring window", {
  one <- data.frame(
    game_id = as.character(seq_len(10)),
    game_date = as.Date(c("2004-12-31", "2005-01-01", "2005-01-02", "2005-01-03",
                          "2005-01-04", "2010-01-01", "2010-01-01", "2010-01-02",
                          "2010-01-02", "2026-01-01")),
    season_start = c(2004L, rep(2005L, 4), rep(2010L, 4), 2026L),
    season_complete = c(rep(TRUE, 9), FALSE),
    site_class = c(rep("home", 7), "neutral", "home", "home"),
    home_win = c(0, 1, 1, 1, 0, 1, 0, 1, 0, 0),
    p_home = rep(c(0.4, 0.6), 5),
    expected_margin = rep(c(-1, 1), 5),
    margin = rep(c(-2, 3), 5)
  )
  games <- do.call(rbind, lapply(c("MLB", "NBA", "NFL", "NHL"), function(lg) {
    transform(one, league = lg)
  }))
  result <- validation_table(games)$summary
  # Four eligible historical games: 3 wins. Both Jan 1 games see 3/4;
  # Jan 2 sees 4/6, except its neutral game gets 1/2.
  expected_p <- c(3/4, 3/4, 1/2, 4/6)
  y <- c(1, 0, 1, 0)
  expected_loss <- -mean(y * log(expected_p) + (1 - y) * log(1 - expected_p))
  expect_equal(result$n, rep(4L, 4))
  expect_equal(result$logloss_naive_home, rep(expected_loss, 4))
  expect_equal(result$brier_naive_home, rep(mean((expected_p - y)^2), 4))
  # Future outcomes and input order cannot change the scored predictions.
  games$home_win[games$season_start == 2026L] <- 1
  expect_equal(validation_table(games[nrow(games):1, ])$summary, result)
})

test_that("IQR intervals refit pooling and retain repeated clusters separately", {
  means <- c(-0.4, -0.1, 0.1, 0.4)
  df <- data.frame(
    home_franchise = rep(LETTERS[1:4], each = 8),
    leftover_margin = rep(means, each = 8) + rep(c(-1, 1), 16)
  )
  label <- "pooling-regression"
  B <- 500L
  result <- boot_shrunk_iqr(df, label, B)
  expect_equal(result$estimate, 0)
  expect_gt(result$hi, 0)

  # Independent moment calculation: every drawn team has 8 games and
  # within-team variance 8/7, hence variance of its mean is 1/7.
  # Calculate the bootstrap statistic without fitting or constructing games.
  set.seed(label_seed(label))
  oracle <- replicate(B, {
    m <- means[sample.int(4, 4, replace = TRUE)]
    tau2 <- max(0, stats::var(m) - 1/7)
    stats::IQR(m) * tau2 / (tau2 + 1/7)
  })
  expect_equal(c(result$lo, result$hi),
               unname(stats::quantile(oracle, c(0.025, 0.975))))
  expect_equal(boot_shrunk_iqr(df, label, B), result)
  expect_true(is.na(boot_shrunk_iqr(df[df$home_franchise != "D", ], label, B)$hi))
})

test_that("IQR cluster draws preserve unequal team sample sizes", {
  sizes <- c(8L, 16L, 24L, 32L)
  means <- c(-1, -0.2, 0.4, 2)
  df <- data.frame(
    home_franchise = rep(LETTERS[1:4], sizes),
    leftover_margin = rep(means, sizes) + rep(c(-1, 1), sum(sizes)/2)
  )
  # Each team's sum of squared deviations is its number of games.
  moment_iqr <- function(ix) {
    n <- sizes[ix]
    m <- means[ix]
    mu <- sum(n * m) / sum(n)
    s2 <- sum(n) / sum(n - 1)
    tau2 <- max(0, (sum(n * (m - mu)^2) - (length(n) - 1) * s2) /
                  (sum(n) - sum(n^2) / sum(n)))
    stats::IQR(mu + (m - mu) * tau2 / (tau2 + s2/n))
  }
  B <- 200L
  label <- "unequal-team-sizes"
  result <- boot_shrunk_iqr(df, label, B)
  set.seed(label_seed(label))
  oracle <- replicate(B, moment_iqr(sample.int(4, 4, replace = TRUE)))
  expect_equal(result$estimate, moment_iqr(1:4))
  expect_equal(c(result$lo, result$hi),
               unname(stats::quantile(oracle, c(0.025, 0.975))))
})

test_that("equal-sized pooling uses sample variance and preserves zero noise", {
  means <- c(-2, -1, 1, 2)
  df <- data.frame(home_franchise = rep(LETTERS[1:4], each = 8),
                   leftover_margin = rep(means, each = 8) + rep(c(-1, 1), 16))
  expect_equal(shrink_one_season(df)$tau2, var(means) - 1/7)
  df$leftover_margin <- rep(means, each = 8)
  fitted <- shrink_one_season(df)
  expect_equal(fitted$tau2, var(means))
  expect_equal(fitted$rows$leftover_shrunk, means)
})

test_that("suspended outcomes cannot enter baseline history until the next day", {
  df <- data.frame(
    game_date = as.Date(c("2020-04-10", "2020-04-11", "2020-05-01", "2020-08-31", "2020-09-01")),
    outcome_date = as.Date(c("2020-04-10", "2020-08-31", "2020-05-01", "2020-08-31", "2020-09-01")),
    home_win = c(0, 1, 0, 0, 0), site_class = "home"
  )
  expect_equal(naive_home_prob(df), c(0.5, 0, 0, 0, 0.25))
  df$home_win[2] <- 0
  expect_equal(naive_home_prob(df), c(0.5, 0, 0, 0, 0))
  df$site_class[3] <- "neutral"
  expect_equal(naive_home_prob(df)[3], 0.5)
  df$outcome_date <- df$game_date
  expect_equal(naive_home_prob(df), naive_home_prob(df[, names(df) != "outcome_date"]))
})

test_that("validation scores only outcomes available as of the study date", {
  one <- data.frame(
    game_id = as.character(1:4), game_date = as.Date("2010-04-01") + 0:3,
    outcome_date = c(as.Date("2010-04-01") + 0:1, study_as_of, study_as_of + 1),
    season_start = 2010L, season_complete = TRUE, site_class = "home",
    home_win = c(1, 0, 1, 0), p_home = c(.6, .4, .7, .1),
    expected_margin = c(1, -1, 2, -9), margin = c(3, -2, 4, -20)
  )
  games <- do.call(rbind, lapply(c("MLB", "NBA", "NFL", "NHL"), function(lg) transform(one, league = lg)))
  result <- validation_table(games)$summary
  expect_equal(result$n, rep(3L, 4))
  expect_equal(result$margin_rmse, rep(sqrt(3), 4))
  expect_equal(result$logloss_model, rep(-mean(log(c(.6, .6, .7))), 4))
})
