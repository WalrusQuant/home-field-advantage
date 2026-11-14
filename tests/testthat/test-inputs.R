source(file.path(root, "R/pull_mlb.R"))
source(file.path(root, "R/pull_nba.R"))
source(file.path(root, "R/pull_nhl.R"))

test_that("Retrosheet outcomes become available on completion, not start", {
  expect_equal(
    retrosheet_outcome_date(c("20210411", "20210412", "20210413"),
                           c("20210831,NYC20,0,0,7", "", NA)),
    as.Date(c("2021-08-31", "2021-04-12", "2021-04-13")))
  expect_equal(retrosheet_outcome_date("20230513", "20230514,WAS11,0,1,13"),
               as.Date("2023-05-14"))
  expect_error(retrosheet_outcome_date("20210411", "unknown"), "completion")
  expect_error(retrosheet_outcome_date("20210411", "20210410,NYC20"), "completion")
})

nba_input_fixture <- function() {
  data.frame(game_id = 1:6, game_date = "2024-01-01", season = 2024,
             season_type = 2, type_abbreviation = "STD",
             home_abbreviation = "BOS", away_abbreviation = "NYK",
             home_score = c(100, 45, 0, 20, 30, 101),
             away_score = c(90, 40, 0, 15, 25, 99),
             status_type_completed = c("TRUE", "FALSE", "FALSE", NA, "unknown", "1"),
             venue_full_name = "TD Garden", neutral_site = FALSE, attendance = 19000)
}

test_that("NBA admits completed games only, including empty results", {
  x <- nba_input_fixture()
  expect_equal(tidy_nba(x)$game_id, c("NBA-1", "NBA-6"))
  expect_equal(nrow(tidy_nba(x[2:5, ])), 0)
  x$status_type_completed <- NULL
  expect_error(tidy_nba(x), "status_type_completed")
})

nhl_input_fixture <- function() {
  data.frame(game_id = as.character(1:6), game_type = "R", game_date = as.Date("2024-01-01"),
             home_team_abbr = "BOS", away_team_abbr = "NYR", home_score = 1,
             away_score = 0, venue = "TD Garden", season = 2024, season_full = "20232024",
             game_state = c("OFF", "FINAL", "LIVE", "FUT", NA, "unknown"),
             neutral_site = FALSE, venue_tz = "America/New_York")
}

test_that("NHL rejects nonterminal scores in release and API-only branches", {
  x <- nhl_input_fixture()
  for (branch in c("release", "api")) {
    raw <- list(release = x[0, ], api = x[0, ])
    raw[[branch]] <- x
    expect_setequal(tidy_nhl(raw)$game_id, c("NHL-1", "NHL-2"))
    raw[[branch]] <- x[3:6, ]
    expect_equal(nrow(tidy_nhl(raw)), 0)
    raw[[branch]]$game_state <- NULL
    expect_error(tidy_nhl(raw), "game_state")
  }
  # A stale nonterminal release must not hide a final API record.
  release <- x[1, ]; release$game_state <- "LIVE"
  expect_equal(tidy_nhl(list(release = release, api = x[1, ]))$game_id, "NHL-1")
})

mlb_api_input_fixture <- function() {
  data.frame(game_pk = 1:5, game_type = "R", official_date = "2024-06-26",
             teams_home_score = 1, teams_away_score = 4,
             teams_home_team_name = "Boston Red Sox", teams_away_team_name = "Toronto Blue Jays",
             venue_name = "Fenway Park", status_abstract_game_state = "Final",
             status_detailed_state = c("Final", "Final", "Postponed", NA, "Final"),
             resume_date = c(NA, "2024-08-26T18:05:00Z", NA, NA, NA))
}

test_that("MLB API cannot silently substitute original dates for unknown completion", {
  x <- mlb_api_input_fixture()
  # Duplicate records for the resumed game must all be excluded.
  x$game_pk[5] <- 2
  expect_message(out <- tidy_mlb_api(x, 2024), "completion dates are unverified")
  expect_equal(out$game_id, "MLBAPI-1-1")
  expect_equal(out$outcome_date, as.Date("2024-06-26"))
  expect_equal(out$game_date, out$outcome_date)
  expect_message(expect_null(tidy_mlb_api(x[2, ], 2024)), "completion dates are unverified")
  x$resume_date <- NULL
  expect_error(tidy_mlb_api(x, 2024), "metadata unavailable")
})
