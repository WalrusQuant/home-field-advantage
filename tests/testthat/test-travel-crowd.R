test_that("haversine distance from Toronto to Buffalo is about 60 miles", {
  places <- load_places()
  tor <- places[places$place_id == "toronto", ]
  buf <- places[places$place_id == "buffalo", ]
  miles <- haversine_miles(tor$lat, tor$lon, buf$lat, buf$lon)
  expect_gt(miles, 50)
  expect_lt(miles, 80)
})

test_that("Denver and Salt Lake are altitude venues and Phoenix is not", {
  places <- load_places()
  expect_true(places$denver_utah[places$place_id == "denver"])
  expect_true(places$high_elevation[places$place_id == "salt_lake"])
  expect_false(places$denver_utah[places$place_id == "phoenix"])
  expect_false(places$high_elevation[places$place_id == "phoenix"])
  expect_true(places$high_elevation[places$place_id == "mexico_city"])
  expect_false(places$denver_utah[places$place_id == "mexico_city"])
})

test_that("every franchise place and time zone resolves", {
  places <- load_places()
  table <- load_franchise_table()
  missing <- setdiff(table$place_id, places$place_id)
  expect_length(missing, 0)
  expect_true(all(places$tz %in% OlsonNames()))
})

test_that("eastward travel from Los Angeles to New York is positive", {
  cross <- tz_cross_hours("America/Los_Angeles", as.Date("2024-06-15"), "America/New_York", as.Date("2024-06-16"))
  expect_equal(cross, 3)
  same <- tz_cross_hours("America/Chicago", as.Date("2024-06-15"), "America/Chicago", as.Date("2024-11-15"))
  expect_equal(same, 0)
})

test_that("a road trip and a home stand are counted, and a neutral game breaks both", {
  site <- c("home", "home", "road", "road", "neutral", "road", "home")
  tn <- trip_numbers(site)
  expect_equal(tn$stand, c(1L, 2L, 0L, 0L, 0L, 0L, 1L))
  expect_equal(tn$trip, c(0L, 0L, 1L, 2L, 0L, 1L, 0L))
})

test_that("days between Monday and Thursday are two rest days", {
  rest <- as.integer(as.Date("2024-09-12") - as.Date("2024-09-09")) - 1L
  expect_equal(rest, 2L)
})

test_that("Retrosheet-style dates and ISO dates both parse", {
  expect_equal(parse_date("20240320"), as.Date("2024-03-20"))
  expect_equal(parse_date("2024-03-20"), as.Date("2024-03-20"))
})

test_that("2020 NFL crowd rules match the compiled spectator table", {
  expect_equal(nfl_2020_crowd("LV", 1, 1)$crowd, "no_fans")
  expect_equal(nfl_2020_crowd("DAL", 1, 1)$crowd, "reduced")
  expect_equal(nfl_2020_crowd("BAL", 1, 1)$crowd, "no_fans")
  expect_equal(nfl_2020_crowd("BAL", 8, 4)$crowd, "reduced")
  den <- nfl_2020_crowd(c("DEN", "DEN", "DEN"), c(1, 2, 8), c(1, 2, 6))
  expect_equal(den$crowd, c("no_fans", "reduced", "no_fans"))
  expect_equal(den$crowd_detail[1], "family_only")
  expect_equal(nfl_2020_crowd("WAS", 9, 5)$crowd, "reduced")
  expect_equal(nfl_2020_crowd("WAS", 12, 6)$crowd, "no_fans")
  expect_equal(nfl_2020_crowd("NO", 6, 3)$crowd, "no_fans")
  expect_equal(nfl_2020_crowd("NO", 7, 4)$crowd, "reduced")
})

test_that("abbreviations survive a move", {
  expect_equal(map_abbrev("NFL", c("SD", "OAK", "STL")), c("LAC", "LV", "LAR"))
  expect_equal(map_abbrev("NBA", c("NJ", "SEA", "NOH")), c("BKN", "OKC", "NOP"))
  expect_equal(map_abbrev("NHL", c("ATL", "PHX")), c("WPG", "UTA"))
  expect_equal(map_abbrev("MLB", c("CHA", "FLO", "MON", "OAK")), c("CWS", "MIA", "WSH", "ATH"))
  expect_equal(
    franchise_place_id("MLB", c("ATH", "ATH"), c(2024L, 2025L)),
    c("oakland", "sacramento")
  )
  expect_equal(
    franchise_place_id("NBA", c("GSW", "GSW"), c(2018L, 2019L)),
    c("oakland", "san_francisco")
  )
})
