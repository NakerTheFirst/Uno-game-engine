# uno — Uno Card Game Engine

An R package implementing a full Uno engine with interactive play, Rcpp-powered
simulation, and a Shiny dashboard.

## Quick start

```r
# Install dependencies
install.packages(c("R6", "Rcpp", "shiny", "ggplot2", "DT",
                   "dplyr", "tibble", "purrr", "tidyr", "scales"))

# Build & install from source
devtools::install_local(".")

# Launch the Shiny dashboard
library(uno)
run_app()
```

## Package architecture

| File | OOP system | Responsibility |
|------|-----------|----------------|
| `01-card.R` | R6 | Card value object — colour, type, value, legality check |
| `02-deck.R` | R6 | 108-card draw pile — shuffle, draw, reshuffle from discard |
| `03-player.R` | R6 | Hand management, strategy label, point counting |
| `04-game-config.R` | S4 | Validated game setup — n_players, hand_size, house rules |
| `05-game.R` | R6 | Game orchestrator — turn order, card effects, win detection |
| `06-results.R` | S3 | `GameResult` + `Leaderboard` with `print()`/`summary()` |
| `07-simulate-r.R` | — | Pure-R simulation loop (benchmark reference) |
| `08-simulate-cpp.R` | — | Rcpp engine wrapper + multi-combo study |
| `src/uno_sim.cpp` | Rcpp | C++ simulation — ~100× faster than pure R |
| `inst/app/app.R` | Shiny | Three-tab dashboard: Play / Simulate / Leaderboard |

## House rules (professor feedback)

The `GameConfig` S4 object exposes two rule toggles. These can be set in the
Shiny app UI or in code:

```r
cfg <- methods::new("GameConfig",
  n_players  = 3L,
  hand_size  = 7L,
  stacking   = TRUE,   # +2 stacks on +2; +4 stacks on +4
  force_play = TRUE,   # must play if a legal card exists
  seed       = NA_integer_
)
```

| Toggle | Default | House rule |
|--------|---------|-----------|
| `stacking` | `FALSE` | Players may stack +2/+4 instead of drawing |
| `force_play` | `TRUE` | Standard: must play if able |

The Shiny **Play** tab has a "Allow +2/+4 stacking" checkbox so players can
toggle this per session without touching any code.

## Strategies & human/robot randomness (professor feedback)

Three bot strategies are implemented (R game engine and Rcpp C++ engine):

| Code | Behaviour |
|------|-----------|
| `"random"` | Plays any legal card uniformly at random |
| `"aggressive"` | Prefers action cards: +4 > +2 > Skip > Reverse > Wild > number |
| `"colour_matcher"` | Prefers cards that keep the active colour; otherwise random |

In the Shiny **Play** tab, the human player occupies seat 1. Bots occupy
seats 2–4. Each bot independently follows its chosen strategy. The human
turn is never auto-played — the player clicks their card.

**How randomness interacts with strategies:**

- When a bot plays a *wild* or *wild+4*, it picks the colour it holds the
  most of (dominant colour). This is deterministic given the hand, so seeds
  make results reproducible.
- A fixed `seed` in `GameConfig` (or in `simulate_games(seed=...)`) makes
  the entire game sequence reproducible — useful for debugging or teaching.
- In the Shiny simulation tab, the seed field lets students reproduce
  specific runs.

## Running tests

```r
devtools::test()
```

Tests cover:
- Deck composition (exactly 108 cards, correct counts per colour/type)  
- Card legality (`matches()`) — same colour, same value, same action type, wild  
- Player hand operations, scoring  
- `GameConfig` slot validation  
- Game rules: skip, reverse, draw-two, stacking, wild colour, illegal-play rejection  
- `GameResult` / `Leaderboard` construction and `summary()`  

## Benchmark

```r
library(microbenchmark)
microbenchmark(
  Rcpp = uno::simulate_games(1000L, c("random","random")),
  R    = uno::simulate_games_r(20L,  c("random","random")),
  times = 3L
)
```

Typical result: Rcpp is **80–150× faster** than the pure-R loop.
