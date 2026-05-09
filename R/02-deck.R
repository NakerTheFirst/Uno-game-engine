#' Deck class
#'
#' Owns the draw pile and (delegated) the discard pile reshuffle logic.
#' The deck does NOT know about players, turns, or rules — only the
#' physical management of cards.
#'
#' A standard Uno deck has 108 cards:
#'   - For each of red/yellow/green/blue:
#'       1 zero, 2 of each 1-9 (= 1 + 18 = 19 number cards)
#'       2 skips, 2 reverses, 2 draw-twos (= 6 action cards)
#'     -> 25 per colour x 4 = 100
#'   - 4 wilds + 4 wild-draw-fours = 8
#'   - Total: 108
#'
#' @export
Deck <- R6::R6Class(
  "Deck",
  public = list(

    #' @description Build a standard 108-card Uno deck and shuffle it.
    initialize = function(seed = NULL) {
      if (!is.null(seed)) set.seed(seed)
      private$draw_pile <- private$build_standard_deck()
      self$shuffle()
    },

    #' @description Shuffle the draw pile in place.
    shuffle = function() {
      n <- length(private$draw_pile)
      if (n > 1) {
        private$draw_pile <- private$draw_pile[sample.int(n)]
      }
      invisible(self)
    },

    #' @description Draw one card off the top of the draw pile. Returns
    #' the Card. If the draw pile is empty, asks for a reshuffle from
    #' the discard pile (passed in by the caller — Deck does not own
    #' the discard pile because Game does).
    draw = function(reshuffle_from = NULL) {
      if (self$size() == 0) {
        if (is.null(reshuffle_from) || length(reshuffle_from) == 0) {
          stop("Draw pile is empty and no discard pile provided to reshuffle from")
        }
        # Caller is responsible for keeping the top discard out.
        # Reset wild colours back to black so they're reusable.
        recycled <- lapply(reshuffle_from, function(card) {
          if (card$type %in% c("wild", "wild_draw_four")) {
            Card$new(colour = "black", type = card$type)
          } else {
            card
          }
        })
        private$draw_pile <- recycled
        self$shuffle()
      }
      top <- private$draw_pile[[1]]
      private$draw_pile <- private$draw_pile[-1]
      top
    },

    #' @description Draw n cards (used for dealing).
    draw_n = function(n, reshuffle_from = NULL) {
      replicate(n, self$draw(reshuffle_from), simplify = FALSE)
    },

    #' @description How many cards are left in the draw pile.
    size = function() length(private$draw_pile),

    #' @description Inspect (read-only) the draw pile. Returns a list of Cards.
    cards = function() private$draw_pile,

    #' @description
    #' Replace the draw pile with the given list of cards. Used by Game
    #' for the initial-flip reinsert dance and (carefully) for tests.
    #' Not part of normal gameplay.
    set_cards = function(cards) {
      stopifnot(is.list(cards),
                all(vapply(cards, inherits, logical(1), what = "Card")))
      private$draw_pile <- cards
      invisible(self)
    }
  ),

  private = list(
    draw_pile = NULL,

    build_standard_deck = function() {
      colours <- c("red", "yellow", "green", "blue")
      cards <- list()

      for (col in colours) {
        # One zero
        cards[[length(cards) + 1L]] <- Card$new(col, value = 0L, type = "number")
        # Two of each 1-9
        for (v in 1:9) {
          cards[[length(cards) + 1L]] <- Card$new(col, value = v, type = "number")
          cards[[length(cards) + 1L]] <- Card$new(col, value = v, type = "number")
        }
        # Two each of skip, reverse, draw_two
        for (t in c("skip", "reverse", "draw_two")) {
          cards[[length(cards) + 1L]] <- Card$new(col, type = t)
          cards[[length(cards) + 1L]] <- Card$new(col, type = t)
        }
      }

      # 4 wilds, 4 wild_draw_fours
      for (i in 1:4) {
        cards[[length(cards) + 1L]] <- Card$new("black", type = "wild")
        cards[[length(cards) + 1L]] <- Card$new("black", type = "wild_draw_four")
      }

      stopifnot("Deck must have exactly 108 cards" = length(cards) == 108)
      cards
    }
  )
)
