# Abbreviation crosswalks and the city a franchise called home in a given season.
# Place coordinates live in data-raw/places.csv (Wikidata, cited there).

load_places <- function() {
  places <- read.csv(path_in("data-raw", "places.csv"), stringsAsFactors = FALSE)
  places$lat <- as.numeric(places$lat)
  places$lon <- as.numeric(places$lon)
  places$elev_m <- as.numeric(places$elev_m)
  places$elev_ft <- places$elev_m * 3.28084
  places$denver_utah <- places$region %in% c("CO", "UT") | places$place_id %in% c("denver", "salt_lake")
  places$high_elevation <- places$denver_utah | (!is.na(places$elev_ft) & places$elev_ft >= 4000)
  places
}

load_franchise_table <- function() {
  read.csv(path_in("data-raw", "franchise_places.csv"), stringsAsFactors = FALSE)
}

load_venue_patterns <- function() {
  patterns <- read.csv(path_in("data-raw", "venue_patterns.csv"), stringsAsFactors = FALSE)
  patterns$force_neutral <- toupper(patterns$force_neutral) %in% c("TRUE", "T", "1")
  patterns
}

abbrev_aliases <- function() {
  list(
    MLB = c(
      ANA = "LAA", CAL = "LAA", LAA = "LAA", ARI = "ARI", ATL = "ATL", BAL = "BAL",
      BOS = "BOS", CHA = "CWS", CHW = "CWS", CWS = "CWS", CHN = "CHC", CHC = "CHC",
      CIN = "CIN", CLE = "CLE", COL = "COL", DET = "DET", HOU = "HOU", KCA = "KC",
      KC = "KC", KCR = "KC", LAN = "LAD", LAD = "LAD", LA = "LAD", MIA = "MIA",
      FLO = "MIA", FLA = "MIA", MIL = "MIL", MIN = "MIN", MON = "WSH", WAS = "WSH",
      WSH = "WSH", WSN = "WSH", NYA = "NYY", NYY = "NYY", NYN = "NYM", NYM = "NYM",
      OAK = "ATH", ATH = "ATH", PHI = "PHI", PIT = "PIT", SDN = "SD", SD = "SD",
      SDP = "SD", SEA = "SEA", SFN = "SF", SF = "SF", SFG = "SF", SLN = "STL",
      STL = "STL", TBA = "TB", TB = "TB", TBR = "TB", TEX = "TEX", TOR = "TOR",
      WSA = "TEX", WS2 = "TEX"
    ),
    NBA = c(
      ATL = "ATL", BOS = "BOS", BKN = "BKN", BRK = "BKN", NJ = "BKN", NJN = "BKN",
      CHA = "CHA", CHO = "CHA", CHB = "CHA", CHI = "CHI", CLE = "CLE", DAL = "DAL",
      DEN = "DEN", DET = "DET", GSW = "GSW", GS = "GSW", HOU = "HOU", IND = "IND",
      LAC = "LAC", LAL = "LAL", MEM = "MEM", MIA = "MIA", MIL = "MIL", MIN = "MIN",
      NOP = "NOP", NO = "NOP", NOH = "NOP", NOK = "NOP", NOO = "NOP", NYK = "NYK",
      NY = "NYK", OKC = "OKC", SEA = "OKC", ORL = "ORL", PHI = "PHI", PHX = "PHX",
      PHO = "PHX", POR = "POR", SAC = "SAC", SAS = "SAS", SA = "SAS", TOR = "TOR",
      UTA = "UTA", UTAH = "UTA", WAS = "WAS", WSH = "WAS"
    ),
    NFL = c(
      ARI = "ARI", ARZ = "ARI", ATL = "ATL", BAL = "BAL", BUF = "BUF", CAR = "CAR",
      CHI = "CHI", CIN = "CIN", CLE = "CLE", CLV = "CLE", DAL = "DAL", DEN = "DEN",
      DET = "DET", GB = "GB", GNB = "GB", HOU = "HOU", IND = "IND", JAX = "JAX",
      JAC = "JAX", KC = "KC", KAN = "KC", LV = "LV", OAK = "LV", LAC = "LAC",
      SD = "LAC", LAR = "LAR", LA = "LAR", STL = "LAR", MIA = "MIA", MIN = "MIN",
      NE = "NE", NWE = "NE", NO = "NO", NOR = "NO", NYG = "NYG", NYJ = "NYJ",
      PHI = "PHI", PIT = "PIT", SF = "SF", SFO = "SF", SEA = "SEA", TB = "TB",
      TAM = "TB", TEN = "TEN", WAS = "WAS", WSH = "WAS"
    ),
    NHL = c(
      ANA = "ANA", BOS = "BOS", BUF = "BUF", CGY = "CGY", CAR = "CAR", CHI = "CHI",
      COL = "COL", CBJ = "CBJ", DAL = "DAL", DET = "DET", EDM = "EDM", FLA = "FLA",
      LAK = "LAK", LA = "LAK", MIN = "MIN", MTL = "MTL", NSH = "NSH", NJD = "NJD",
      NJ = "NJD", NYI = "NYI", NYR = "NYR", OTT = "OTT", PHI = "PHI", PIT = "PIT",
      SJS = "SJS", SJ = "SJS", STL = "STL", TBL = "TBL", TB = "TBL", TOR = "TOR",
      VAN = "VAN", VGK = "VGK", SEA = "SEA", WSH = "WSH", WAS = "WSH",
      ATL = "WPG", WPG = "WPG", PHX = "UTA", ARI = "UTA", UTA = "UTA"
    )
  )
}

mlb_full_names <- function() {
  c(
    "Arizona Diamondbacks" = "ARI",
    "Atlanta Braves" = "ATL",
    "Baltimore Orioles" = "BAL",
    "Boston Red Sox" = "BOS",
    "Chicago Cubs" = "CHC",
    "Chicago White Sox" = "CWS",
    "Cincinnati Reds" = "CIN",
    "Cleveland Guardians" = "CLE",
    "Cleveland Indians" = "CLE",
    "Colorado Rockies" = "COL",
    "Detroit Tigers" = "DET",
    "Houston Astros" = "HOU",
    "Kansas City Royals" = "KC",
    "Los Angeles Angels" = "LAA",
    "Los Angeles Dodgers" = "LAD",
    "Miami Marlins" = "MIA",
    "Florida Marlins" = "MIA",
    "Milwaukee Brewers" = "MIL",
    "Minnesota Twins" = "MIN",
    "New York Mets" = "NYM",
    "New York Yankees" = "NYY",
    "Athletics" = "ATH",
    "Oakland Athletics" = "ATH",
    "Philadelphia Phillies" = "PHI",
    "Pittsburgh Pirates" = "PIT",
    "San Diego Padres" = "SD",
    "San Francisco Giants" = "SF",
    "Seattle Mariners" = "SEA",
    "St. Louis Cardinals" = "STL",
    "Tampa Bay Rays" = "TB",
    "Tampa Bay Devil Rays" = "TB",
    "Texas Rangers" = "TEX",
    "Toronto Blue Jays" = "TOR",
    "Washington Nationals" = "WSH",
    "Montreal Expos" = "WSH"
  )
}

map_abbrev <- function(league, abbr) {
  key <- toupper(trimws(as.character(abbr)))
  alias <- abbrev_aliases()[[league]]
  mapped <- unname(alias[key])
  mapped[is.na(alias[key])] <- NA_character_
  # Named lookup returns NA for unknown keys; keep that.
  ifelse(key %in% names(alias), unname(alias[key]), NA_character_)
}

map_mlb_name <- function(name) {
  table <- mlb_full_names()
  unname(table[as.character(name)])
}

require_mapped <- function(league, abbr, franchise, season_start, when) {
  bad <- is.na(franchise) & !is.na(season_start) & season_start >= warmup_start_year
  if (any(bad)) {
    sample <- unique(abbr[bad])
    stop(
      league, " has unmapped abbreviations at ", when, ": ",
      paste(utils::head(sample, 12), collapse = ", ")
    )
  }
  franchise
}

franchise_place_id <- function(league, franchise, season_start, table = NULL) {
  if (is.null(table)) {
    table <- load_franchise_table()
  }
  n <- length(franchise)
  if (length(league) == 1L) league <- rep(league, n)
  if (length(season_start) == 1L) season_start <- rep(season_start, n)
  out <- rep(NA_character_, n)
  for (i in seq_along(franchise)) {
    if (is.na(franchise[i]) || is.na(season_start[i])) next
    hit <- table[
      table$league == league[i] &
        table$franchise == franchise[i] &
        season_start[i] >= table$start_min &
        season_start[i] <= table$start_max,
      ,
      drop = FALSE
    ]
    if (nrow(hit) == 0L) next
    width <- hit$start_max - hit$start_min
    out[i] <- hit$place_id[which.min(width)]
  }
  out
}

match_place_id <- function(city, region, places) {
  city_key <- tolower(trimws(as.character(city)))
  city_key <- ifelse(city_key %in% c("st. paul", "st paul"), "saint paul", city_key)
  region_key <- toupper(trimws(as.character(region)))
  region_key <- ifelse(region_key %in% names(region_aliases()), unname(region_aliases()[region_key]), region_key)
  place_city <- tolower(places$city)
  out <- rep(NA_character_, length(city_key))
  for (i in seq_along(city_key)) {
    if (!nzchar(city_key[i]) || is.na(city_key[i])) next
    hit <- which(place_city == city_key[i])
    if (length(hit) == 0L) next
    if (length(hit) == 1L || is.na(region_key[i]) || !nzchar(region_key[i])) {
      out[i] <- places$place_id[hit[1]]
      next
    }
    region_hit <- hit[toupper(places$region[hit]) == region_key[i]]
    if (length(region_hit) == 1L) out[i] <- places$place_id[region_hit]
  }
  out
}

region_aliases <- function() {
  c(ONT = "ON", QUE = "QC", ALB = "AB", MAN = "MB", SAS = "SK", BC = "BC")
}

pattern_place <- function(venue, patterns) {
  venue <- as.character(venue)
  place <- rep(NA_character_, length(venue))
  force <- rep(FALSE, length(venue))
  for (i in seq_len(nrow(patterns))) {
    hit <- !is.na(venue) & grepl(patterns$pattern[i], venue, ignore.case = TRUE)
    # First match wins so a specific pattern listed earlier beats a broad one.
    new <- hit & is.na(place)
    place[new] <- patterns$place_id[i]
    force[new] <- isTRUE(patterns$force_neutral[i])
  }
  list(place_id = place, force_neutral = force)
}
