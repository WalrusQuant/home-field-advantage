# Study window and model settings.
# Hyperparameters were fixed before looking at home-edge results.
# They are round on purpose and are not tuned to move a league's estimate.

study_as_of <- as.Date("2026-11-14")

# Reported seasons are those that start in this year or later.
report_start_year <- 2010L

# Earlier seasons warm up ratings and are not in the reported tables.
warmup_start_year <- 2005L

# Raw home win rate only, where a long public file is easy to rebuild.
long_raw_start <- c(NFL = 1999L, MLB = 1970L)

# Last completed season start year as of study_as_of.
# MLB's 2026 regular season was over. NFL 2026 and the 2026-27 NBA and NHL
# seasons were still in progress on 14 November 2026.
completed_start_year <- c(MLB = 2026L, NFL = 2025L, NBA = 2025L, NHL = 2025L)

# Exponential half-life, in days, of a past game's weight.
half_life_days <- c(NBA = 150, NFL = 200, NHL = 150, MLB = 120)

# Ridge penalties in games of weight. Team ratings shrink toward 0.
# The home term shrinks toward home_prior.
lambda_team <- 4
lambda_home <- 10

# Weak prior for the home term, in the league's scoring unit.
home_prior <- c(NBA = 2.5, NFL = 2.5, NHL = 0.25, MLB = 0.20)

sigma_init <- c(NBA = 12, NFL = 13.5, NHL = 1.7, MLB = 3.8)
sigma_floor <- c(NBA = 8, NFL = 8, NHL = 1.0, MLB = 2.0)

# Pregame |neutral margin| / sigma. Fixed before the tier chart was drawn.
tier_breaks <- c(0, 0.25, 0.75, Inf)
tier_labels <- c("Coin-flip", "Modest gap", "Big gap")

# Distance from the franchise's usual city. Above this, the game is not a
# true home game. Fifty miles keeps a cross-town move in the home sample and
# leaves Toronto-Buffalo (about 60 miles) outside it.
off_campus_miles <- 50

boot_B <- 1000L

scoring_unit <- c(NBA = "points", NFL = "points", NHL = "goals", MLB = "runs")

nba_bubble_start <- as.Date("2020-07-01")
nba_bubble_end <- as.Date("2020-10-15")
