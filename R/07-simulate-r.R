#' Simulate Uno games in pure R (slow but correct reference).
#'
#' @param n number of games to simulate
#' @param strategies character vector of strategies, one per player
#' @param hand_size starting hand size (default 7)
#' @param stacking allow +2/+4 stacking
#' @param seed optional integer seed
#' @return a tibble with one row per game
#' @export
simulate_games_r <- function(n,
                             strategies = c("random", "random"),
                             hand_size  = 7L,
                             stacking   = FALSE,
                             seed       = NULL) {
  stopifnot(
    is.numeric(n), n >= 1,
    length(strategies) >= 2L, length(strategies) <= 4L
  )
  if (!is.null(seed)) set.seed(seed)

  cfg <- methods::new("GameConfig",
    n_players  = length(strategies),
    hand_size  = as.integer(hand_size),
    stacking   = stacking,
    force_play = TRUE,
    seed       = NA_integer_
  )

  player_specs <- lapply(seq_along(strategies), function(i) {
    list(name = sprintf("P%d_%s", i, strategies[i]),
         strategy = strategies[i])
  })

  results <- purrr::map_dfr(seq_len(n), function(i) {
    g <- Game$new(cfg, player_specs)
    # Cap turns to avoid degenerate infinite loops in pathological RNG.
    max_turns <- 5000L
    t <- 0L
    while (!g$is_over() && t < max_turns) {
      g$auto_turn()
      t <- t + 1L
    }
    if (!g$is_over()) {
      # Treat as no-winner; rare. Skip.
      return(tibble::tibble(
        game        = i,
        winner      = NA_character_,
        winner_strategy = NA_character_,
        n_turns     = t,
        cards_left  = NA_integer_
      ))
    }
    res <- g$result()
    tibble::tibble(
      game            = i,
      winner          = res$winner,
      winner_strategy = res$strategies[res$winner_idx],
      n_turns         = res$n_turns,
      cards_left      = sum(res$hand_sizes)
    )
  })
  results
}
