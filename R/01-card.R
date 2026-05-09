#' Card class
#'
#' Represents a single Uno card. Cards are conceptually value objects:
#' once created, their colour/value/type don't change. The class is R6
#' for consistency with the rest of the engine, but no method mutates
#' state — methods are pure queries.
#'
#' @section Card grammar:
#' - `type` is one of: "number", "skip", "reverse", "draw_two",
#'   "wild", "wild_draw_four"
#' - `colour` is one of: "red", "yellow", "green", "blue", or "black"
#'   (black is reserved for wilds before a colour is chosen)
#' - `value` is 0-9 for number cards, NA for action cards
#'
#' Compatibility (whether this card can legally be played on top of
#' another) lives here, but action-card *effects* (skipping, drawing,
#' reversing) belong to the Game class — Card does not know about
#' players or turns.
#'
#' @export
Card <- R6::R6Class(
  "Card",
  public = list(
    colour = NULL,
    value  = NULL,
    type   = NULL,

    #' @description Create a card. Validates inputs.
    initialize = function(colour, value = NA_integer_, type = "number") {
      valid_colours <- c("red", "yellow", "green", "blue", "black")
      valid_types   <- c("number", "skip", "reverse", "draw_two",
                         "wild", "wild_draw_four")

      stopifnot(
        "colour must be one of red/yellow/green/blue/black" =
          colour %in% valid_colours,
        "type must be a valid Uno card type" =
          type %in% valid_types
      )

      # Wilds must start as "black" (no colour chosen yet).
      # Non-wild cards must NOT be black.
      is_wild <- type %in% c("wild", "wild_draw_four")
      if (is_wild && colour != "black") {
        stop("Wild cards must be created with colour = 'black'")
      }
      if (!is_wild && colour == "black") {
        stop("Non-wild cards cannot have colour 'black'")
      }

      # Number cards need a value 0-9; action cards have NA.
      if (type == "number") {
        stopifnot(
          "Number cards need an integer value 0-9" =
            !is.na(value) && value >= 0 && value <= 9
        )
        self$value <- as.integer(value)
      } else {
        self$value <- NA_integer_
      }

      self$colour <- colour
      self$type   <- type
    },

    #' @description Is this card a wild (either kind)?
    is_wild = function() {
      self$type %in% c("wild", "wild_draw_four")
    },

    #' @description Is this an action card (anything that isn't a number)?
    is_action = function() {
      self$type != "number"
    },

    #' @description
    #' Can this card be legally played on top of `top_card`, given the
    #' currently-active colour `current_colour` (which matters when the
    #' top card is a wild that's been coloured-in)?
    #'
    #' Wilds are always playable. A wild_draw_four has the additional
    #' real-Uno rule that you can only play it if you have no card of
    #' the current colour — but we enforce that in Game (it needs to
    #' inspect the player's hand), not here.
    matches = function(top_card, current_colour) {
      stopifnot(inherits(top_card, "Card"))
      if (self$is_wild()) return(TRUE)
      if (self$colour == current_colour) return(TRUE)
      if (self$type == "number" && top_card$type == "number" &&
          !is.na(self$value) && !is.na(top_card$value) &&
          self$value == top_card$value) {
        return(TRUE)
      }
      if (self$type != "number" && self$type == top_card$type) {
        return(TRUE)
      }
      FALSE
    },

    #' @description Short label for printing/UI.
    label = function() {
      if (self$type == "number")          sprintf("%s %d", self$colour, self$value)
      else if (self$type == "skip")       sprintf("%s Skip", self$colour)
      else if (self$type == "reverse")    sprintf("%s Reverse", self$colour)
      else if (self$type == "draw_two")   sprintf("%s +2", self$colour)
      else if (self$type == "wild")       "Wild"
      else if (self$type == "wild_draw_four") "Wild +4"
    },

    #' @description Custom print.
    print = function(...) {
      cat("<Card>", self$label(), "\n", sep = " ")
      invisible(self)
    }
  )
)

#' @export
format.Card <- function(x, ...) x$label()

#' @export
print.Card <- function(x, ...) {
  x$print()
  invisible(x)
}
