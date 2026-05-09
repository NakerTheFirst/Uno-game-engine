#' Construct a GameResult (S3)
#'
#' Snapshot of a finished game's outcome.
#' @export
new_game_result <- function(winner, winner_idx, player_names, strategies,
                            hand_sizes, hand_points, n_turns, log) {
  stopifnot(
    is.character(winner), length(winner) == 1L,
    is.numeric(winner_idx), length(winner_idx) == 1L,
    is.character(player_names),
    is.character(strategies),
    length(player_names) == length(strategies),
    length(hand_sizes) == length(player_names),
    length(hand_points) == length(player_names)
  )
  obj <- list(
    winner       = winner,
    winner_idx   = as.integer(winner_idx),
    player_names = player_names,
    strategies   = strategies,
    hand_sizes   = as.integer(hand_sizes),
    hand_points  = as.integer(hand_points),
    n_turns      = as.integer(n_turns),
    log          = log,
    timestamp    = Sys.time()
  )
  class(obj) <- "GameResult"
  obj
}

#' @export
print.GameResult <- function(x, ...) {
  cat("<GameResult>\n")
  cat(sprintf("  Winner: %s (player %d)\n", x$winner, x$winner_idx))
  cat(sprintf("  Turns played: %d\n", x$n_turns))
  cat("  Final standings:\n")
  ord <- order(x$hand_sizes)
  for (i in ord) {
    cat(sprintf("    %s [%s]: %d cards left, %d points\n",
                x$player_names[i], x$strategies[i],
                x$hand_sizes[i], x$hand_points[i]))
  }
  invisible(x)
}

#' @export
summary.GameResult <- function(object, ...) {
  out <- tibble::tibble(
    player    = object$player_names,
    strategy  = object$strategies,
    cards_left = object$hand_sizes,
    points    = object$hand_points,
    is_winner = seq_along(object$player_names) == object$winner_idx
  )
  attr(out, "n_turns") <- object$n_turns
  attr(out, "winner")  <- object$winner
  out
}

# ---- Leaderboard ----

#' Construct an empty Leaderboard.
#' @export
new_leaderboard <- function() {
  obj <- list(
    games   = list(),  # list of GameResult
    started = Sys.time()
  )
  class(obj) <- "Leaderboard"
  obj
}

#' Add a GameResult to a Leaderboard.
#' @export
add_game_result <- function(lb, gr) {
  stopifnot(inherits(lb, "Leaderboard"),
            inherits(gr, "GameResult"))
  lb$games[[length(lb$games) + 1L]] <- gr
  lb
}

#' Update leaderboard with a result (alias).
#' @export
update_leaderboard <- add_game_result

# Helper: turn the leaderboard's list-of-results into a tidy tibble.
.lb_long <- function(lb) {
  if (length(lb$games) == 0) {
    return(tibble::tibble(
      player    = character(0),
      strategy  = character(0),
      cards_left = integer(0),
      points    = integer(0),
      is_winner = logical(0),
      game_id   = integer(0)
    ))
  }
  rows <- purrr::map_dfr(seq_along(lb$games), function(i) {
    g <- lb$games[[i]]
    tibble::tibble(
      player     = g$player_names,
      strategy   = g$strategies,
      cards_left = g$hand_sizes,
      points     = g$hand_points,
      is_winner  = seq_along(g$player_names) == g$winner_idx,
      game_id    = i
    )
  })
  rows
}

#' @export
print.Leaderboard <- function(x, ...) {
  n_games <- length(x$games)
  cat(sprintf("<Leaderboard> %d games played\n", n_games))
  if (n_games == 0L) {
    cat("  (no games yet)\n")
    return(invisible(x))
  }
  s <- summary(x)
  cat("  Player rankings:\n")
  for (i in seq_len(nrow(s))) {
    cat(sprintf("    %2d. %-15s %3d wins / %3d games (%5.1f%%)\n",
                i, s$player[i], s$wins[i], s$games[i], 100 * s$win_rate[i]))
  }
  invisible(x)
}

#' @export
summary.Leaderboard <- function(object, ...) {
  long <- .lb_long(object)
  if (nrow(long) == 0) {
    return(tibble::tibble(
      player = character(0), games = integer(0),
      wins = integer(0), win_rate = numeric(0)
    ))
  }
  out <- long |>
    dplyr::group_by(.data = _, player) |>
    dplyr::summarise(
      games    = dplyr::n(),
      wins     = sum(is_winner),
      win_rate = mean(is_winner),
      .groups  = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(win_rate), dplyr::desc(wins))
  out
}
