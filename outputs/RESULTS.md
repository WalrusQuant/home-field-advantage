Numbers below are read from `outputs/tables/` after the pipeline run. Intervals are 95% cluster-bootstrap intervals by home team.

### Coverage

- **MLB**: 39,772 games in seasons starting 2010–2026 (39,605 of them in the true-home sample). Completed through 2026.
- **NBA**: 19,301 games in seasons starting 2010–2026-27 (18,447 of them in the true-home sample). Completed through 2025-26. Partial: 2026-27.
- **NFL**: 4,312 games in seasons starting 2010–2026 (4,242 of them in the true-home sample). Completed through 2025. Partial: 2026.
- **NHL**: 19,463 games in seasons starting 2010–2026-27 (19,388 of them in the true-home sample). Completed through 2025-26. Partial: 2026-27.

### Model validation

Log loss is on decisive games in completed seasons from 2010 on, using the pregame win probability that includes the home term. Lower is better. The coin-flip baseline is 0.5. The naive home baseline is the league's home win rate before that date.
- **MLB** (39,771 games, 1 ties left out of the scores): model log loss 0.682, naive home 0.693, coin flip 0.693. Brier 0.244 vs 0.249 vs 0.250. Calibration error 0.025. Correlation of expected and actual margin 0.17, RMSE 4.33 runs.
- **NBA** (19,118 games, 0 ties left out of the scores): model log loss 0.616, naive home 0.686, coin flip 0.693. Brier 0.214 vs 0.245 vs 0.250. Calibration error 0.013. Correlation of expected and actual margin 0.40, RMSE 13.05 points.
- **NFL** (4,162 games, 13 ties left out of the scores): model log loss 0.632, naive home 0.696, coin flip 0.693. Brier 0.221 vs 0.247 vs 0.250. Calibration error 0.021. Correlation of expected and actual margin 0.38, RMSE 13.41 points.
- **NHL** (19,152 games, 0 ties left out of the scores): model log loss 0.673, naive home 0.690, coin flip 0.693. Brier 0.240 vs 0.248 vs 0.250. Calibration error 0.010. Correlation of expected and actual margin 0.21, RMSE 2.43 goals.

### Chart A, early window versus the latest completed season

- **NBA** 2025-26: raw home win rate 55.3 (49.2 to 61.2), leftover margin 1.68 (0.61 to 2.75) points, win-probability edge 5.4 (2.5 to 8.4) percentage points, team-edge IQR 1.79 (1.01 to 2.62) points. From the 2010–14 mean to 2025-26, raw went down (-3.5 percentage points), leftover margin went down (-1.24), and the win-probability edge went down (-3.8 percentage points).
- **NFL** 2025: raw home win rate 53.8 (46.2 to 60.9), leftover margin 2.13 (0.22 to 3.97) points, win-probability edge 3.6 (-2.9 to 9.9) percentage points, team-edge IQR 2.37 (1.45 to 3.06) points. From the 2010–14 mean to 2025, raw went down (-3.8 percentage points), leftover margin went down (-0.54), and the win-probability edge went down (-3.8 percentage points).
- **MLB** 2026: raw home win rate 52.9 (50.3 to 55.6), leftover margin 0.05 (-0.09 to 0.20) runs, win-probability edge 2.9 (1.5 to 4.7) percentage points, team-edge IQR 0.00 (0.00 to 0.00) runs. From the 2010–14 mean to 2026, raw went down (-0.9 percentage points), leftover margin went down (-0.07), and the win-probability edge went down (-0.8 percentage points).
- **NHL** 2025-26: raw home win rate 52.1 (48.1 to 55.4), leftover margin 0.13 (-0.00 to 0.26) goals, win-probability edge 2.2 (-1.0 to 5.1) percentage points, team-edge IQR 0.00 (0.00 to 0.00) goals. From the 2010–14 mean to 2025-26, raw went down (-2.4 percentage points), leftover margin went down (-0.14), and the win-probability edge went down (-2.4 percentage points).

### Chart B, home edge by pregame gap

Coin-flip is a neutral gap under 0.25 residual SD, modest is 0.25 to 0.75, and big is 0.75 or more. Completed true-home games.
- **NBA** (points): Coin-flip margin 2.47 (2.19 to 2.75), win-probability edge 8.4 (7.3 to 9.5) pp (n=7,628); Modest gap margin 2.38 (2.03 to 2.70), win-probability edge 7.1 (6.1 to 8.1) pp (n=8,895); Big gap margin 2.53 (1.87 to 3.16), win-probability edge 4.5 (2.5 to 6.5) pp (n=1,748).
- **NFL** (points): Coin-flip margin 2.06 (1.40 to 2.79), win-probability edge 5.3 (3.1 to 7.8) pp (n=1,801); Modest gap margin 1.76 (1.13 to 2.35), win-probability edge 5.0 (2.7 to 7.2) pp (n=1,970); Big gap margin 3.42 (2.17 to 4.64), win-probability edge 8.8 (4.8 to 12.2) pp (n=341).
- **MLB** (runs): Coin-flip margin 0.11 (0.08 to 0.15), win-probability edge 3.6 (3.0 to 4.1) pp (n=29,801); Modest gap margin 0.07 (-0.01 to 0.15), win-probability edge 3.1 (2.2 to 4.0) pp (n=9,785); Big gap margin -0.02 (-2.63 to 2.99), win-probability edge 12.6 (-24.6 to 46.6) pp (n=19).
- **NHL** (goals): Coin-flip margin 0.25 (0.21 to 0.29), win-probability edge 4.0 (3.0 to 4.9) pp (n=12,419); Modest gap margin 0.26 (0.19 to 0.32), win-probability edge 4.3 (2.9 to 5.6) pp (n=6,564); Big gap margin 0.89 (0.29 to 1.60), win-probability edge 13.7 (5.6 to 23.7) pp (n=96).

### Chart C

- **NBA**: correlation of raw home win rate and shrunk leftover margin across team-seasons is 0.61 (n=464).
- **NFL**: correlation of raw home win rate and shrunk leftover margin across team-seasons is 0.52 (n=512).
- **MLB**: correlation of raw home win rate and shrunk leftover margin across team-seasons is 0.10 (n=508).
- **NHL**: correlation of raw home win rate and shrunk leftover margin across team-seasons is 0.34 (n=494).

### 2020 contrast, leftover margin

- **NBA, 2017–19, fans** (n=2,370): 2.51 (1.74 to 3.33) points.
- **NBA, 2019–20 bubble (neutral)** (n=88): 1.71 (-1.07 to 4.78) points.
- **NBA, 2020–21 empty** (n=544): 0.27 (-1.36 to 2.06) points.
- **NBA, 2020–21 reduced** (n=455): 1.38 (-0.68 to 3.41) points.
- **NBA, 2021–23, fans** (n=2,355): 2.07 (1.31 to 2.83) points.
- **NFL, 2018–19 seasons** (n=504): 1.04 (-0.13 to 2.25) points.
- **NFL, 2020 empty** (n=145): -0.42 (-2.93 to 1.79) points.
- **NFL, 2020 reduced** (n=108): 0.90 (-1.81 to 3.56) points.
- **NFL, 2021–22 seasons** (n=534): 1.91 (0.86 to 2.90) points.
- **MLB, 2018–19 seasons** (n=4,854): 0.08 (-0.01 to 0.17) runs.
- **MLB, 2020 empty home** (n=872): 0.16 (-0.11 to 0.42) runs.
- **MLB, 2020 makeshift home** (n=26): 1.06 (-1.30 to 3.42) runs.
- **MLB, 2021–22 seasons** (n=4,834): 0.11 (-0.00 to 0.22) runs.
- **NHL, 2018–20 seasons** (n=2,342): 0.28 (0.21 to 0.36) goals.
- **NHL, 2020–21 restricted (attendance unknown)** (n=866): 0.26 (0.07 to 0.46) goals.
- **NHL, 2021–23 seasons** (n=2,615): 0.22 (0.13 to 0.30) goals.

### Data gaps

- **NHL attendance.** The NHL schedule feed has no game attendance. 2020-21 is labeled restricted from the season calendar, not from turnstile counts.
- **NFL attendance.** nflverse schedules have no attendance. 2020 empty versus reduced games follow the spectator policies compiled on the Wikipedia 2020 NFL season page, retrieved 2026-11-14.
- **MLB attendance zeros.** Outside 2020, a Retrosheet attendance of 0 is treated as unknown. In 2020 it is treated as an empty ballpark.
- **MLB seasons after the Retrosheet logs.** Seasons not yet in the Retrosheet game logs, if any, come from baseballr::mlb_schedule and have no attendance.
- **Coordinates.** Share of games without coordinates: 0.0%. Travel rows that need those games are dropped.
- **Partial seasons.** NFL 2026 and the 2026-27 NBA and NHL seasons were in progress on 2026-11-14. They stay in the data and are open points on chart A. MLB 2026 regular season is treated as complete.
- **Overtime and shootouts.** Margins use the published final score, including overtime. An NHL shootout win is whatever one-goal margin the NHL feed records.
- **Within-metro travel.** Usual-home coordinates are city or arena-neighborhood points. Trips under about 50 miles are local travel, not surveyed stadium-to-stadium routes.
- **Pregame ratings.** Intervals treat the pregame ratings as fixed. They do not include uncertainty from the rating model itself. Ratings for games on the same date ignore one another.

### Chart files

- `outputs/charts/A_home_edge_nba_nfl.png`
- `outputs/charts/A_home_edge_over_time.png`
- `outputs/charts/A_long_raw_home_win.png`
- `outputs/charts/A_measures_indexed.png`
- `outputs/charts/B_home_edge_by_strength_tier.png`
- `outputs/charts/C_raw_vs_leftover_team_season.png`
- `outputs/charts/D_2020_crowd.png`
- `outputs/charts/E_rest_trip_back_to_back.png`
- `outputs/charts/E_travel_altitude.png`
- `outputs/charts/V_calibration.png`

