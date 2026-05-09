#' Player class
#'
#' Owns a player's hand and identity. Knows how to add/remove cards
#' and report which cards in the hand are legal given the current
#' top-of-discard. Does NOT mutate the deck or game — Game asks the
#' player to play a card and the player hands it over.
#'
#' Strategy is a string label ("human", "random", "aggressive",
#' "colour_matcher") used by Game/simulator to pick a card on this
#' player's turn. Humans have strategy = "human" and Game will not
#' auto-play for them.
#'
#' Player class
#'
#' @export
Player <- R6::R6Class(
  "Player",
  public = list(
    name     = NULL,
    strategy = NULL,
    score    = 0L,
    
    initialize = function(name, strategy = "human") {
      stopifnot(
        "Player name must be a non-empty string" =
          is.character(name) && length(name) == 1 && nchar(name) > 0,
        "Strategy must be one of human/random/aggressive/colour_matcher" =
          strategy %in% c("human", "random", "aggressive", "colour_matcher")
      )
      
      self$name     <- name
      self$strategy <- strategy
      private$.hand <- list()
    },
    
    receive = function(card) {
      stopifnot(inherits(card, "Card"))
      private$.hand[[length(private$.hand) + 1L]] <- card
      invisible(self)
    },
    
    receive_many = function(cards) {
      for (card in cards) {
        self$receive(card)
      }
      invisible(self)
    },
    
    remove_at = function(i) {
      stopifnot(
        "Index out of bounds" =
          length(i) == 1 && i >= 1L && i <= length(private$.hand)
      )
      
      card <- private$.hand[[i]]
      private$.hand[[i]] <- NULL
      card
    },
    
    hand = function() {
      private$.hand
    },
    
    hand_size = function() {
      length(private$.hand)
    },
    
    legal_indices = function(top_card, current_colour) {
      if (length(private$.hand) == 0) {
        return(integer(0))
      }
      
      keep <- vapply(
        private$.hand,
        function(card) card$matches(top_card, current_colour),
        logical(1)
      )
      
      which(keep)
    },
    
    has_colour = function(colour) {
      if (length(private$.hand) == 0) {
        return(FALSE)
      }
      
      any(vapply(
        private$.hand,
        function(card) card$colour == colour && !card$is_wild(),
        logical(1)
      ))
    },
    
    hand_points = function() {
      if (length(private$.hand) == 0) {
        return(0L)
      }
      
      pts <- vapply(
        private$.hand,
        function(card) {
          if (card$type == "number") {
            as.integer(card$value)
          } else if (card$type %in% c("skip", "reverse", "draw_two")) {
            20L
          } else {
            50L
          }
        },
        integer(1)
      )
      
      sum(pts)
    }
  ),
  
  private = list(
    .hand = NULL
  )
)
