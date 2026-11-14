source(file.path(root, "R/context.R"))

test_that("future modal venues cannot change model home eligibility", {
  for (distance in c(10, 50, 51, NA_real_)) {
    games <- data.frame(
      league = "NBA", home_franchise = "T", season_start = 2018:2020,
      venue_key = c("A", "B", "A"), venue_place = c("A", "B", "A"),
      usual_place = "A", miles_from_usual = c(0, distance, 0),
      lat = c(40, if (is.na(distance)) NA_real_ else 40, 40), lon = -75,
      feed_neutral = FALSE, pattern_neutral = FALSE, bubble = FALSE
    )
    classify <- function(g) {
      merged <- merge(g, modal_venues(g), by = c("league", "home_franchise", "season_start"))
      classified <- classify_sites(merged)
      classified[classified$season_start == 2019, ]
    }
    prefix <- classify(games[1:2, ])
    full <- classify(games)
    expect_equal(prefix$model_neutral, full$model_neutral)
    expected <- is.na(distance) || distance > 50
    expect_equal(full$model_neutral, expected)
    expect_equal(full$site_class, if (is.na(distance)) "alternate" else if (distance > 50) "makeshift" else "home")
    if (is.finite(distance) && distance > 50) expect_equal(prefix$site_class, "alternate")
  }
})

test_that("missing coordinates are alternate regardless of modal metadata", {
  games <- data.frame(
    league = "NBA", season_start = 2020L, miles_from_usual = c(NA, NA, NA, 60),
    lat = c(NA, 40, 40, NA), lon = c(-75, NA, -75, -75),
    venue_key = "A", modal_key = c("A", "B", NA, "A"), makeshift_season = TRUE,
    feed_neutral = FALSE, pattern_neutral = FALSE, bubble = FALSE
  )
  expect_equal(classify_sites(games)$site_class, rep("alternate", 4))
  games$feed_neutral[1] <- TRUE
  games$bubble[2] <- TRUE
  expect_equal(classify_sites(games)$site_class, c("neutral", "bubble", "alternate", "alternate"))
})

test_that("travel compares geographic offsets on the destination date", {
  expect_equal(tz_cross_hours("America/New_York", as.Date("2024-03-09"),
                             "America/Chicago", as.Date("2024-03-11")), -1)
  expect_equal(tz_cross_hours("America/Chicago", as.Date("2024-03-09"),
                             "America/New_York", as.Date("2024-03-11")), 1)
  expect_equal(tz_cross_hours("America/New_York", as.Date("2024-11-02"),
                             "America/Chicago", as.Date("2024-11-04")), -1)
  expect_equal(tz_cross_hours("US/Eastern", as.Date("2024-03-09"),
                             "America/Detroit", as.Date("2024-03-11")), 0)
  expect_equal(tz_cross_hours("America/Phoenix", as.Date("2024-03-09"),
                             "America/Denver", as.Date("2024-03-11")), 1)
})

test_that("attendance without reference remains unknown unless directly classifiable", {
  games <- data.frame(
    league = c(rep("NBA", 6), rep("MLB", 5)),
    season_start = c(2018, rep(2020, 5), 2019, 2021, 2021, 2021, 2020),
    site_class = "home", feed_neutral = FALSE,
    venue_key = c("NBA-old", "NBA-new", "NBA-empty", "NBA-bubble", "NBA-old", "NBA-old",
                  "MLB-old", "MLB-new", "MLB-old", "MLB-old", "MLB-new"),
    attendance = c(20000, 1000, 0, NA, 4000, 15000, 20000, 1000, 4000, 15000, 0),
    bubble = c(FALSE, FALSE, FALSE, TRUE, rep(FALSE, 7))
  )
  expect_equal(assign_crowd(games)$crowd,
               c("normal", "unknown", "no_fans", "bubble", "reduced", "normal",
                 "normal", "unknown", "reduced", "normal", "no_fans"))
  # Entirely absent reference data must also work.
  expect_equal(assign_crowd(games[c(2, 3, 8, 11), ])$crowd,
               c("unknown", "no_fans", "unknown", "no_fans"))
})
