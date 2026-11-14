# Shared helpers. No machine-specific paths are written into outputs.

root_dir <- function() {
  root <- getOption("home_edge_root")
  if (is.null(root) || !nzchar(root)) {
    stop("Set options(home_edge_root = ...) to the repository root before sourcing project code.")
  }
  root
}

path_in <- function(...) file.path(root_dir(), ...)

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

say <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), "  ", paste0(...))
}

user_agent <- function() {
  "home-edge-study/0.1 (public reproducible research)"
}

# Evaluate `expr` only when the cache file is missing.
cache_rds <- function(path, expr) {
  if (file.exists(path) && Sys.getenv("FORCE_REFRESH", "") != "1") {
    say("Cache hit ", path)
    return(readRDS(path))
  }
  ensure_dir(dirname(path))
  value <- force(expr)
  saveRDS(value, path)
  value
}

download_file <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0 && Sys.getenv("FORCE_REFRESH", "") != "1") {
    return(dest)
  }
  ensure_dir(dirname(dest))
  curl::curl_download(
    url,
    dest,
    quiet = TRUE,
    handle = curl::new_handle(useragent = user_agent())
  )
  dest
}

parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(format(x, "%Y-%m-%d")))
  }
  raw <- substr(as.character(x), 1, 10)
  compact <- grepl("^[0-9]{8}$", raw)
  out <- as.Date(rep(NA_character_, length(raw)))
  if (any(compact)) out[compact] <- as.Date(raw[compact], format = "%Y%m%d")
  if (any(!compact)) out[!compact] <- as.Date(raw[!compact])
  out
}

label_seed <- function(label) {
  bytes <- utf8ToInt(paste0(label))
  seed <- 20261114
  for (byte in bytes) {
    seed <- (seed * 131 + byte) %% 2147483646
  }
  as.integer(seed + 1L)
}

# Cluster bootstrap of a mean. Clusters are resampled, then every game in a
# drawn cluster is kept, so larger clubs keep their weight.
boot_mean <- function(x, cluster, label, B = boot_B) {
  ok <- is.finite(x) & !is.na(cluster)
  x <- x[ok]
  cluster <- as.character(cluster[ok])
  n <- length(x)
  if (n == 0L) {
    return(data.frame(n = 0L, estimate = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  estimate <- mean(x)
  parts <- split(seq_along(x), cluster)
  sums <- vapply(parts, function(ii) sum(x[ii]), numeric(1))
  counts <- vapply(parts, length, integer(1))
  k <- length(sums)
  if (k < 2L || n < 5L) {
    se <- stats::sd(x) / sqrt(n)
    if (!is.finite(se)) se <- NA_real_
    return(data.frame(
      n = n,
      estimate = estimate,
      lo = estimate - 1.96 * se,
      hi = estimate + 1.96 * se
    ))
  }
  set.seed(label_seed(label))
  draw <- matrix(sample.int(k, B * k, replace = TRUE), nrow = B)
  boot <- rowSums(matrix(sums[draw], nrow = B)) / rowSums(matrix(counts[draw], nrow = B))
  qs <- stats::quantile(boot, c(0.025, 0.975), names = FALSE, na.rm = TRUE)
  data.frame(n = n, estimate = estimate, lo = unname(qs[1]), hi = unname(qs[2]))
}

boot_iqr <- function(x, label, B = boot_B) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4L) {
    return(data.frame(n = n, estimate = NA_real_, lo = NA_real_, hi = NA_real_))
  }
  estimate <- stats::IQR(x)
  set.seed(label_seed(label))
  boot <- replicate(B, stats::IQR(sample(x, n, replace = TRUE)))
  qs <- stats::quantile(boot, c(0.025, 0.975), names = FALSE, na.rm = TRUE)
  data.frame(n = n, estimate = estimate, lo = unname(qs[1]), hi = unname(qs[2]))
}

write_table <- function(data, name) {
  dest <- path_in("outputs", "tables", name)
  ensure_dir(dirname(dest))
  readr::write_csv(data, dest)
  say("Wrote ", dest)
  dest
}

empty_game_skeleton <- function() {
  data.frame(
    league = character(),
    game_id = character(),
    game_date = as.Date(character()),
    season_start = integer(),
    season_end = integer(),
    week = integer(),
    home_abbr = character(),
    away_abbr = character(),
    home_franchise = character(),
    away_franchise = character(),
    venue = character(),
    venue_city = character(),
    venue_region = character(),
    venue_key = character(),
    home_score = numeric(),
    away_score = numeric(),
    feed_neutral = logical(),
    attendance = numeric(),
    source = character(),
    stringsAsFactors = FALSE
  )
}
