# Great-circle distance and the within-season rest / trip state.

haversine_miles <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 3958.7613 * asin(pmin(1, sqrt(pmax(a, 0))))
}

# site is "home", "road", or "neutral". Neutral breaks both a stand and a trip.
trip_numbers <- function(site) {
  n <- length(site)
  stand <- integer(n)
  trip <- integer(n)
  s <- 0L
  t <- 0L
  for (i in seq_len(n)) {
    if (identical(site[i], "home")) {
      s <- s + 1L
      t <- 0L
    } else if (identical(site[i], "road")) {
      t <- t + 1L
      s <- 0L
    } else {
      s <- 0L
      t <- 0L
    }
    stand[i] <- s
    trip[i] <- t
  }
  list(stand = stand, trip = trip)
}

normalize_tz <- function(tz) {
  tz <- as.character(tz)
  mapped <- c(
    "US/Eastern" = "America/New_York",
    "US/Central" = "America/Chicago",
    "US/Mountain" = "America/Denver",
    "US/Pacific" = "America/Los_Angeles",
    "US/Arizona" = "America/Phoenix",
    "US/Hawaii" = "Pacific/Honolulu"
  )
  hit <- unname(mapped[tz])
  ifelse(!is.na(hit), hit, tz)
}

tz_offset_hours <- function(tz, date) {
  tz <- normalize_tz(tz)
  out <- rep(NA_real_, length(tz))
  known <- tz %in% OlsonNames()
  ok <- !is.na(tz) & tz != "" & !is.na(date) & known
  if (!any(ok)) {
    return(out)
  }
  for (zone in unique(tz[ok])) {
    idx <- which(ok & tz == zone)
    stamp <- as.POSIXct(paste(as.character(date[idx]), "12:00:00"), tz = zone)
    out[idx] <- as.numeric(format(stamp, "%z")) / 100
  }
  out
}

# East is positive: America/Los_Angeles (-7) to America/New_York (-4) is +3.
tz_cross_hours <- function(prev_tz, prev_date, tz, date) {
  prev_tz <- normalize_tz(prev_tz)
  tz <- normalize_tz(tz)
  same <- !is.na(prev_tz) & !is.na(tz) & prev_tz == tz
  # Compare both clocks on arrival day, so elapsed DST changes are not travel.
  cross <- tz_offset_hours(tz, date) - tz_offset_hours(prev_tz, date)
  cross[same] <- 0
  cross
}
