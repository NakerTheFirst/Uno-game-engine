#' Simulate Uno games using the Rcpp engine.
#'
#' @param n number of games
#' @param strategies character vector of strategies (random/aggressive/colour_matcher)
#' @param hand_size starting hand size
#' @param stacking allow +2/+4 stacking
#' @param seed integer seed
#' @return a tibble with columns game, winner, winner_strategy, n_turns, cards_left
#' @export
simulate_games <- function(n,
                           strategies = c("random", "random"),
                           hand_size  = 7L,
                           stacking   = FALSE,
                           seed       = 1L) {
  stopifnot(
    is.numeric(n), n >= 1,
    length(strategies) >= 2L, length(strategies) <= 4L,
    all(strategies %in% c("random", "aggressive", "colour_matcher"))
  )
  code_map <- c(random = 0L, aggressive = 1L, colour_matcher = 2L)
  strat_int <- unname(code_map[strategies])

  out <- cpp_simulate_games(
    n          = as.integer(n),
    strategies = strat_int,
    hand_size  = as.integer(hand_size),
    stacking   = stacking,
    seed       = as.integer(seed)
  )
  out <- tibble::as_tibble(out)

  # Map winner_strategy code back to label
  rev_map <- setNames(names(code_map), unname(code_map))
  out$winner_strategy <- ifelse(is.na(out$winner_strategy),
                                NA_character_,
                                rev_map[as.character(out$winner_strategy)])
  # Player names
  player_names <- sprintf("P%d_%s", seq_along(strategies), strategies)
  out$winner <- ifelse(is.na(out$winner_idx),
                       NA_character_,
                       player_names[out$winner_idx])
  out[, c("game", "winner", "winner_strategy", "n_turns", "cards_left")]
}

#' Run a multi-strategy simulation study.
#'
#' Plays `n_per_combo` games for each combination of strategies (one
#' per seat). Useful for "is aggressive better than random in 3-player
#' games?" style questions.
#'
#' @param strategy_grid a list of character vectors. Each element is a
#'   strategy assignment to be simulated.
#' @param n_per_combo games per combo
#' @param hand_size hand size
#' @param stacking allow stacking
#' @param seed seed
#' @return tibble with columns: combo_id, strategies, n_turns_mean,
#'   n_turns_sd, win_rate_random, win_rate_aggressive, win_rate_colour_matcher
#' @export
run_simulation_study <- function(strategy_grid,
                                 n_per_combo = 1000L,
                                 hand_size   = 7L,
                                 stacking    = FALSE,
                                 seed        = 1L) {
  stopifnot(is.list(strategy_grid), length(strategy_grid) >= 1L)
  purrr::map_dfr(seq_along(strategy_grid), function(i) {
    s <- strategy_grid[[i]]
    res <- simulate_games(
      n = n_per_combo, strategies = s,
      hand_size = hand_size, stacking = stacking, seed = seed + i
    )
    # Vectorised summary - no loops
    win_table <- table(factor(res$winner_strategy,
                              levels = c("random", "aggressive", "colour_matcher")))
    total <- sum(win_table)
    tibble::tibble(
      combo_id                 = i,
      strategies               = paste(s, collapse = " / "),
      n_players                = length(s),
      games                    = nrow(res),
      n_turns_mean             = mean(res$n_turns),
      n_turns_sd               = sd(res$n_turns),
      win_rate_random          = if (total > 0) win_table[["random"]] / total else NA_real_,
      win_rate_aggressive      = if (total > 0) win_table[["aggressive"]] / total else NA_real_,
      win_rate_colour_matcher  = if (total > 0) win_table[["colour_matcher"]] / total else NA_real_
    )
  })
}
