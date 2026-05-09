#' GameConfig (S4)
#'
#' Strictly-validated configuration for a Uno game. Built once before
#' starting a game; `Game$new()` will only accept a valid GameConfig.
#'
#' Slots:
#' - `n_players`: integer 2-4
#' - `hand_size`: integer 5-10 (default 7)
#' - `stacking`: logical, allow stacking +2/+4 cards
#' - `force_play`: logical, must play if you have a legal card
#' - `seed`: integer or NA, RNG seed for reproducibility
#'
#' @export GameConfig
#' @exportClass GameConfig
GameConfig <- methods::setClass(
  "GameConfig",
  representation(
    n_players  = "integer",
    hand_size  = "integer",
    stacking   = "logical",
    force_play = "logical",
    seed       = "integer"
  ),
  prototype(
    n_players  = 4L,
    hand_size  = 7L,
    stacking   = FALSE,
    force_play = TRUE,
    seed       = NA_integer_
  ),
  validity = function(object) {
    errs <- character(0)
    if (length(object@n_players) != 1L ||
        object@n_players < 2L || object@n_players > 4L) {
      errs <- c(errs, "n_players must be a single integer in 2:4")
    }
    if (length(object@hand_size) != 1L ||
        object@hand_size < 5L || object@hand_size > 10L) {
      errs <- c(errs, "hand_size must be a single integer in 5:10")
    }
    if (length(object@stacking) != 1L) {
      errs <- c(errs, "stacking must be a single logical")
    }
    if (length(object@force_play) != 1L) {
      errs <- c(errs, "force_play must be a single logical")
    }
    if (length(errs) == 0) TRUE else errs
  }
)
