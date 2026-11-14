# Pregame team-strength model.
#
# For a game starting on date D, use only outcomes known before D. outcome_date
# defaults to game_date; suspended games enter history after their completion
# date. Same-day outcomes never inform predictions that day.
#
# The weighted least-squares model, with an L2 penalty, is
#
#   margin = strength_home - strength_away + h * (1 - neutral) + error
#
# margin is the final home score minus the final away score, overtime included.
# neutral games (bubbles, international sites, makeshift homes, one-off
# relocations) do not receive h. Team strengths are penalized toward 0 and h
# toward a weak prior. Past games are down-weighted with a half-life in days.
#
# Because the weight of a game depends on age only through a^(D - d), the
# normal equations can be updated by decaying the running sums between dates,
# without a new pass over the design matrix or exponentially growing weights.
#
# Win probability is Phi(expected margin / sigma), with sigma the weighted
# residual standard deviation from that same pregame fit.

predict_pregame <- function(games,
                            half_life_days,
                            n_teams,
                            lambda_team = 4,
                            lambda_home = 10,
                            home_prior = 0,
                            sigma_init = 12,
                            sigma_floor = 1) {
  if (nrow(games) == 0L) {
    stop("predict_pregame received no games.")
  }
  required <- c("game_date", "home_ix", "away_ix", "margin", "neutral")
  missing <- setdiff(required, names(games))
  if (length(missing)) {
    stop("predict_pregame is missing columns: ", paste(missing, collapse = ", "))
  }
  if (any(!is.finite(games$margin))) {
    stop("predict_pregame margins must be finite. Drop unplayed games first.")
  }
  if (any(games$home_ix < 1L | games$away_ix < 1L | games$home_ix > n_teams | games$away_ix > n_teams)) {
    stop("Team indexes fall outside 1..n_teams.")
  }

  if (!inherits(games$game_date, "Date") || any(!is.finite(games$game_date))) {
    stop("predict_pregame game_date must contain finite Date values.")
  }
  outcome_date <- if ("outcome_date" %in% names(games)) games$outcome_date else games$game_date
  if (!inherits(outcome_date, "Date") || any(!is.finite(outcome_date))) {
    stop("predict_pregame outcome_date must contain finite Date values.")
  }
  if (any(outcome_date < games$game_date)) {
    stop("predict_pregame outcome_date must be on or after game_date.")
  }
  if (anyNA(games$neutral) || any(!games$neutral %in% c(0, 1))) {
    stop("predict_pregame neutral must contain only 0 or 1.")
  }
  if (length(half_life_days) != 1L || !is.finite(half_life_days) || half_life_days <= 0) {
    stop("predict_pregame half_life_days must be finite and positive.")
  }

  n <- nrow(games)
  date_num <- as.integer(games$game_date)
  origin <- min(date_num)
  rel <- date_num - origin
  outcome_rel <- as.integer(outcome_date) - origin
  home <- as.integer(games$home_ix)
  away <- as.integer(games$away_ix)
  margin <- as.numeric(games$margin)
  neutral <- as.integer(games$neutral)
  p <- n_teams + 1L

  out_nm <- rep(NA_real_, n)
  out_em <- rep(NA_real_, n)
  out_pn <- rep(NA_real_, n)
  out_ph <- rep(NA_real_, n)
  out_h <- rep(NA_real_, n)
  out_sigma <- rep(NA_real_, n)
  out_hs <- rep(NA_real_, n)
  out_as <- rep(NA_real_, n)

  gram <- matrix(0, p, p)
  rhs <- rep(0, p)
  ywy <- 0
  sw <- 0

  # Predict before admitting outcomes on each event date. Completion-only dates
  # still update history; decay always measures age since the original start.
  prediction_rows <- split(seq_len(n), rel)
  outcome_rows <- split(seq_len(n), outcome_rel)
  event_dates <- sort(unique(c(rel, outcome_rel)))
  event_dates <- event_dates[event_dates <= max(rel)]

  previous_rel <- 0
  for (drel in event_dates) {
    idx <- prediction_rows[[as.character(drel)]]
    scale <- 2 ^ (-(drel - previous_rel) / half_life_days)
    gram <- scale * gram
    rhs <- scale * rhs
    ywy <- scale * ywy
    sw <- scale * sw
    previous_rel <- drel

    g_now <- gram
    rhs_now <- rhs
    diag(g_now) <- diag(g_now) + c(rep(lambda_team, n_teams), lambda_home)
    rhs_now[p] <- rhs_now[p] + lambda_home * home_prior
    beta <- tryCatch(
      solve(g_now, rhs_now),
      error = function(e) solve(g_now + diag(1e-6, p), rhs_now)
    )

    sw_now <- sw
    if (sw_now >= 20) {
      sse <- ywy - 2 * sum(beta * rhs) + sum(beta * as.vector(gram %*% beta))
      sigma <- sqrt(max(sse, 0) / sw_now)
      if (!is.finite(sigma) || sigma < sigma_floor) sigma <- sigma_floor
    } else {
      sigma <- sigma_init
    }

    h_hat <- beta[p]
    for (i in idx) {
      nm <- beta[home[i]] - beta[away[i]]
      em <- nm + h_hat * (1 - neutral[i])
      out_nm[i] <- nm
      out_em[i] <- em
      out_pn[i] <- stats::pnorm(nm / sigma)
      out_ph[i] <- stats::pnorm(em / sigma)
      out_h[i] <- h_hat
      out_sigma[i] <- sigma
      out_hs[i] <- beta[home[i]]
      out_as[i] <- beta[away[i]]
    }

    for (i in outcome_rows[[as.character(drel)]]) {
      weight <- 2 ^ (-(drel - rel[i]) / half_life_days)
      hi <- home[i]
      ai <- away[i]
      gram[hi, hi] <- gram[hi, hi] + weight
      gram[ai, ai] <- gram[ai, ai] + weight
      gram[hi, ai] <- gram[hi, ai] - weight
      gram[ai, hi] <- gram[ai, hi] - weight
      rhs[hi] <- rhs[hi] + weight * margin[i]
      rhs[ai] <- rhs[ai] - weight * margin[i]
      ywy <- ywy + weight * margin[i] ^ 2
      sw <- sw + weight
      if (neutral[i] == 0L) {
        gram[hi, p] <- gram[hi, p] + weight
        gram[ai, p] <- gram[ai, p] - weight
        gram[p, hi] <- gram[p, hi] + weight
        gram[p, ai] <- gram[p, ai] - weight
        gram[p, p] <- gram[p, p] + weight
        rhs[p] <- rhs[p] + weight * margin[i]
      }
    }
  }

  data.frame(
    neutral_margin = out_nm,
    expected_margin = out_em,
    p_neutral = out_pn,
    p_home = out_ph,
    h_pregame = out_h,
    sigma_pregame = out_sigma,
    home_strength = out_hs,
    away_strength = out_as
  )
}

# One home coefficient per season, fit jointly with team strengths on true
# home games. Descriptive, not pregame: it uses the whole season.
joint_home_term <- function(home_ix, away_ix, margin, n_teams, neutral = FALSE, lambda_team = 1) {
  ok <- is.finite(margin)
  home_ix <- home_ix[ok]
  away_ix <- away_ix[ok]
  margin <- margin[ok]
  if (length(neutral) == 1L) neutral <- rep(neutral, length(ok))
  neutral <- as.logical(neutral[ok])
  if (length(margin) < n_teams + 5L || !any(!neutral)) {
    return(NA_real_)
  }
  p <- n_teams + 1L
  gram <- matrix(0, p, p)
  rhs <- rep(0, p)
  for (i in seq_along(margin)) {
    hi <- home_ix[i]
    ai <- away_ix[i]
    gram[hi, hi] <- gram[hi, hi] + 1
    gram[ai, ai] <- gram[ai, ai] + 1
    gram[hi, ai] <- gram[hi, ai] - 1
    gram[ai, hi] <- gram[ai, hi] - 1
    rhs[hi] <- rhs[hi] + margin[i]
    rhs[ai] <- rhs[ai] - margin[i]
    if (!neutral[i]) {
      gram[hi, p] <- gram[hi, p] + 1
      gram[ai, p] <- gram[ai, p] - 1
      gram[p, hi] <- gram[p, hi] + 1
      gram[p, ai] <- gram[p, ai] - 1
      gram[p, p] <- gram[p, p] + 1
      rhs[p] <- rhs[p] + margin[i]
    }
  }
  diag(gram)[seq_len(n_teams)] <- diag(gram)[seq_len(n_teams)] + lambda_team
  beta <- solve(gram, rhs)
  beta[p]
}

fit_games <- function(games) {
  pred_cols <- c(
    "neutral_margin", "expected_margin", "p_neutral", "p_home",
    "h_pregame", "sigma_pregame", "home_strength", "away_strength"
  )
  for (col in pred_cols) games[[col]] <- NA_real_
  for (lg in c("MLB", "NBA", "NFL", "NHL")) {
    idx <- which(games$league == lg & games$season_start >= warmup_start_year)
    if (!length(idx)) next
    say("Fitting ", lg, " on ", length(idx), " games")
    sub <- games[idx, , drop = FALSE]
    teams <- sort(unique(c(sub$home_franchise, sub$away_franchise)))
    teams <- teams[!is.na(teams) & !startsWith(teams, "UNK-")]
    ix <- setNames(seq_along(teams), teams)
    if (any(!sub$home_franchise %in% teams) || any(!sub$away_franchise %in% teams)) {
      stop(lg, " has an unmapped franchise inside the model window.")
    }
    pred <- predict_pregame(
      data.frame(
        game_date = sub$game_date,
        outcome_date = if ("outcome_date" %in% names(sub)) sub$outcome_date else sub$game_date,
        home_ix = unname(ix[sub$home_franchise]),
        away_ix = unname(ix[sub$away_franchise]),
        margin = sub$home_score - sub$away_score,
        neutral = as.integer(sub$site_class != "home")
      ),
      half_life_days = unname(half_life_days[[lg]]),
      n_teams = length(teams),
      lambda_team = lambda_team,
      lambda_home = lambda_home,
      home_prior = unname(home_prior[[lg]]),
      sigma_init = unname(sigma_init[[lg]]),
      sigma_floor = unname(sigma_floor[[lg]])
    )
    for (col in pred_cols) games[[col]][idx] <- pred[[col]]
  }
  games$leftover_margin <- games$margin - games$neutral_margin
  games$leftover_wp <- games$home_win - games$p_neutral
  games$abs_z <- abs(games$neutral_margin) / games$sigma_pregame
  games$tier <- cut(
    games$abs_z,
    breaks = tier_breaks,
    labels = tier_labels,
    right = FALSE,
    include.lowest = TRUE
  )
  games$in_home_edge <- games$site_class == "home" &
    games$season_start >= report_start_year &
    is.finite(games$neutral_margin)
  games
}
