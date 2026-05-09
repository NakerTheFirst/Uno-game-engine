#' Game class
#'
#' The orchestrator. Owns:
#'   - the players (list of Player)
#'   - the deck (Deck)
#'   - the discard pile (list of Card; top is last element)
#'   - the current turn pointer and direction
#'   - the current active colour (matters when top is a wild)
#'   - pending draw count (when stacking +2/+4)
#'
#' This is the ONLY class that interprets card effects. Card knows
#' "I am a Skip"; Game knows "if a Skip is played, advance the turn
#' pointer twice".
#'
#' Flow per turn:
#'   1. current player picks a card (or chooses to draw)
#'   2. Game validates the play with `play_card()` or `draw_card()`
#'   3. Game applies the card's effect (skip / reverse / draw / wild)
#'   4. Game advances to the next player
#'
#' @export
Game <- R6::R6Class(
  "Game",
  public = list(
    players       = NULL,
    deck          = NULL,
    discard       = NULL,   # list of Card; top = last element
    current_idx   = 1L,     # 1-based index into players
    direction     = 1L,     # +1 clockwise, -1 counter
    active_colour = NULL,   # one of red/yellow/green/blue
    pending_draw  = 0L,     # for stacking +2/+4
    finished      = FALSE,
    winner_idx    = NA_integer_,
    config        = NULL,
    log           = NULL,   # character vector of events

    #' @description Construct and deal a new game.
    #' @param config a GameConfig (S4)
    #' @param player_specs list of list(name=, strategy=)
    initialize = function(config, player_specs) {
      stopifnot(
        "config must be a GameConfig" =
          methods::is(config, "GameConfig"),
        "Number of player_specs must match config@n_players" =
          length(player_specs) == config@n_players
      )
      self$config <- config
      if (!is.na(config@seed)) set.seed(config@seed)

      # Build players
      self$players <- lapply(player_specs, function(s) {
        Player$new(name = s$name, strategy = s$strategy %||% "human")
      })

      # Build deck and deal
      self$deck <- Deck$new()
      for (p in self$players) {
        p$receive_many(self$deck$draw_n(config@hand_size))
      }

      # Flip initial top card. Re-flip if it's a wild_draw_four
      # (real Uno rule: stuff it back in and try again).
      repeat {
        first <- self$deck$draw()
        if (first$type != "wild_draw_four") break
        # put back at a random position
        # (simulate "shuffle it back in")
        private$reinsert_into_deck(first)
      }
      self$discard <- list(first)

      # Active colour: if first card is a wild, pick randomly.
      # If it's a coloured card, use that colour.
      if (first$is_wild()) {
        self$active_colour <- sample(c("red","yellow","green","blue"), 1L)
      } else {
        self$active_colour <- first$colour
      }

      # Initial action card on top: apply its effect to the *first*
      # player (then advance). Standard Uno rule.
      self$current_idx <- 1L
      self$direction   <- 1L
      self$pending_draw <- 0L
      self$log <- character(0)
      private$log_event(sprintf(
        "Game started. Top card: %s. Active colour: %s.",
        first$label(), self$active_colour))

      private$apply_initial_top_effect(first)
    },

    #' @description Get the current player (Player object).
    current_player = function() self$players[[self$current_idx]],

    #' @description Top card of the discard pile.
    top_card = function() self$discard[[length(self$discard)]],

    #' @description
    #' Play a card from the current player's hand. `card_idx` is the
    #' position in their hand; `wild_colour` is required for wilds.
    play_card = function(card_idx, wild_colour = NA_character_) {
      if (self$finished) stop("Game is already finished")
      if (self$pending_draw > 0L) {
        # Player faces a stacked draw. They can only play a stack-able
        # card if stacking is enabled.
        return(private$resolve_pending_draw_play(card_idx, wild_colour))
      }
      private$do_play(card_idx, wild_colour)
    },

    #' @description
    #' Current player draws a card. If they can play it immediately
    #' and force_play is FALSE, they may choose to (caller decides);
    #' default behaviour returns the drawn card and advances the turn.
    #' For action card stacks, this resolves the pending draw.
    draw_card = function() {
      if (self$finished) stop("Game is already finished")

      if (self$pending_draw > 0L) {
        # Take the penalty.
        return(private$take_pending_draw())
      }

      # Normal turn: draw 1.
      drawn <- private$safe_draw()
      self$current_player()$receive(drawn)
      private$log_event(sprintf("%s draws %s.",
                                self$current_player()$name, drawn$label()))

      # If the drawn card is legal, the rules let the player play it
      # immediately. We do NOT auto-play; we return the card and let
      # the caller decide. If they don't play it, they call pass_turn().
      drawn
    },

    #' @description Pass the turn after drawing (if the drawn card
    #' isn't played).
    pass_turn = function() {
      private$advance_turn(1L)
      invisible(self)
    },

    #' @description
    #' Convenience for bots/sim: take one full turn for the current
    #' player using their strategy. Returns invisibly.
    auto_turn = function() {
      if (self$finished) return(invisible(self))
      p <- self$current_player()
      if (p$strategy == "human") {
        stop("auto_turn cannot be called for a human player")
      }

      # Handle pending draws (stacking).
      if (self$pending_draw > 0L) {
        legal <- private$stackable_indices(p)
        if (self$config@stacking && length(legal) > 0L) {
          choice <- private$pick_card(p, legal)
          wc <- if (p$hand()[[choice]]$is_wild()) private$pick_colour(p) else NA_character_
          self$play_card(choice, wild_colour = wc)
        } else {
          private$take_pending_draw()
        }
        return(invisible(self))
      }

      # Normal turn
      legal <- p$legal_indices(self$top_card(), self$active_colour)
      # Real-Uno rule: wild_draw_four only if no card of active colour.
      legal <- private$filter_w4_legality(p, legal)

      if (length(legal) > 0L) {
        choice <- private$pick_card(p, legal)
        wc <- if (p$hand()[[choice]]$is_wild()) private$pick_colour(p) else NA_character_
        self$play_card(choice, wild_colour = wc)
      } else {
        drawn <- self$draw_card()
        # If the drawn card happens to be playable, play it.
        if (drawn$matches(self$top_card(), self$active_colour) &&
            (drawn$type != "wild_draw_four" ||
             !p$has_colour(self$active_colour))) {
          # The drawn card is at the end of the hand
          last_idx <- p$hand_size()
          wc <- if (drawn$is_wild()) private$pick_colour(p) else NA_character_
          self$play_card(last_idx, wild_colour = wc)
        } else {
          self$pass_turn()
        }
      }
      invisible(self)
    },

    #' @description Has the game ended (some player has 0 cards)?
    is_over = function() self$finished,

    #' @description Build a GameResult S3 from the final state.
    result = function() {
      if (!self$finished) stop("Game is not finished")
      hand_sizes <- vapply(self$players, function(p) p$hand_size(), integer(1))
      points     <- vapply(self$players, function(p) p$hand_points(), integer(1))
      names_v    <- vapply(self$players, function(p) p$name, character(1))
      strategies <- vapply(self$players, function(p) p$strategy, character(1))
      new_game_result(
        winner       = names_v[self$winner_idx],
        winner_idx   = self$winner_idx,
        player_names = names_v,
        strategies   = strategies,
        hand_sizes   = hand_sizes,
        hand_points  = points,
        n_turns      = length(self$log),
        log          = self$log
      )
    }
  ),

  private = list(

    log_event = function(msg) {
      self$log <- c(self$log, msg)
    },

    safe_draw = function() {
      tryCatch({
        if (self$deck$size() == 0L) {
          # Reshuffle from discard, keeping the top card out.
          if (length(self$discard) <= 1L) {
            stop("No cards left to draw and no discard to reshuffle")
          }
          top <- self$discard[[length(self$discard)]]
          rest <- self$discard[-length(self$discard)]
          self$discard <- list(top)
          self$deck$draw(reshuffle_from = rest)
        } else {
          self$deck$draw()
        }
      }, error = function(e) {
        stop("draw_card failed: ", conditionMessage(e))
      })
    },

    do_play = function(card_idx, wild_colour) {
      p <- self$current_player()
      stopifnot(
        "card_idx out of range" =
          card_idx >= 1L && card_idx <= p$hand_size()
      )
      card <- p$hand()[[card_idx]]
      if (!card$matches(self$top_card(), self$active_colour)) {
        stop(sprintf("Illegal play: %s does not match top card %s (active colour: %s)",
                     card$label(), self$top_card()$label(), self$active_colour))
      }
      # Wild +4 legality: only if you have no card of the active colour.
      if (card$type == "wild_draw_four" && p$has_colour(self$active_colour)) {
        stop("Cannot play Wild +4 while holding a card of the current colour")
      }
      if (card$is_wild()) {
        if (!wild_colour %in% c("red", "yellow", "green", "blue")) {
          stop("Wild cards require wild_colour to be red/yellow/green/blue")
        }
      }

      # Remove from hand, push onto discard.
      played <- p$remove_at(card_idx)
      # If wild, set its colour for display purposes (but the card
      # object's colour stays "black"; we track active_colour separately).
      self$discard[[length(self$discard) + 1L]] <- played

      # Update active colour
      self$active_colour <- if (played$is_wild()) wild_colour else played$colour

      private$log_event(sprintf("%s plays %s%s.",
        p$name, played$label(),
        if (played$is_wild()) sprintf(" (chooses %s)", wild_colour) else ""))

      # Win check
      if (p$hand_size() == 0L) {
        self$finished   <- TRUE
        self$winner_idx <- self$current_idx
        private$log_event(sprintf("%s wins!", p$name))
        return(invisible(self))
      }

      # Apply card effect
      private$apply_card_effect(played)
      invisible(self)
    },

    apply_card_effect = function(card) {
      switch(card$type,
        "number" = private$advance_turn(1L),
        "skip"   = private$advance_turn(2L),
        "reverse" = {
          self$direction <- -self$direction
          # In a 2-player game, reverse acts like skip
          if (length(self$players) == 2L) {
            private$advance_turn(2L)
          } else {
            private$advance_turn(1L)
          }
        },
        "draw_two" = {
          if (self$config@stacking) {
            self$pending_draw <- self$pending_draw + 2L
            private$advance_turn(1L)
          } else {
            private$advance_turn(1L)
            victim <- self$current_player()
            for (i in 1:2) victim$receive(private$safe_draw())
            private$log_event(sprintf("%s draws 2.", victim$name))
            private$advance_turn(1L)
          }
        },
        "wild" = private$advance_turn(1L),
        "wild_draw_four" = {
          if (self$config@stacking) {
            self$pending_draw <- self$pending_draw + 4L
            private$advance_turn(1L)
          } else {
            private$advance_turn(1L)
            victim <- self$current_player()
            for (i in 1:4) victim$receive(private$safe_draw())
            private$log_event(sprintf("%s draws 4.", victim$name))
            private$advance_turn(1L)
          }
        }
      )
    },

    apply_initial_top_effect = function(first) {
      # Standard Uno: if the flipped card is an action card, the first
      # player is affected by it.
      switch(first$type,
        "number" = invisible(NULL),
        "skip"   = private$advance_turn(1L),
        "reverse" = {
          self$direction <- -self$direction
          # Start play from the "last" player (real rule).
          self$current_idx <- if (self$direction == -1L) length(self$players) else 1L
        },
        "draw_two" = {
          victim <- self$current_player()
          for (i in 1:2) victim$receive(private$safe_draw())
          private$log_event(sprintf("%s draws 2 (initial).", victim$name))
          private$advance_turn(1L)
        },
        "wild" = invisible(NULL),
        "wild_draw_four" = invisible(NULL)  # we re-flipped, can't happen
      )
    },

    advance_turn = function(steps) {
      n <- length(self$players)
      idx <- self$current_idx
      for (i in seq_len(steps)) {
        idx <- ((idx - 1L + self$direction) %% n) + 1L
      }
      self$current_idx <- idx
    },

    reinsert_into_deck = function(card) {
      # Put it back at a random position in the draw pile.
      cards <- self$deck$cards()
      n <- length(cards)
      pos <- sample.int(n + 1L, 1L)
      # We need to rebuild the deck. Simplest: append + shuffle.
      # We can't directly modify private$draw_pile, so we cheat by
      # giving Deck a new internal state via a fresh deck. But that
      # would lose our work. Easier: append and reshuffle.
      cards <- append(cards, list(card), after = pos - 1L)
      # Replace the deck. This is a deliberate Game-internal operation.
      self$deck <- private$deck_from_cards(cards)
    },

    deck_from_cards = function(cards) {
      d <- Deck$new()
      # Overwrite: Deck has no public setter, so we use R6's
      # private member access via $set after construction.
      # Cleanest: extend Deck with a method. We add it via env assign.
      # (See Deck.set_cards added in deck.R below — done with $set on
      # the generator.)
      d$set_cards(cards)
      d
    },

    # ---- Strategies ----
    pick_card = function(player, legal_indices) {
      hand <- player$hand()
      if (player$strategy == "random") {
        if (length(legal_indices) == 1L) return(legal_indices)
        return(sample(legal_indices, 1L))
      }
      if (player$strategy == "aggressive") {
        # Prefer action cards; among them, prefer +4 > +2 > skip > reverse > wild.
        priority <- vapply(legal_indices, function(i) {
          t <- hand[[i]]$type
          switch(t,
            "wild_draw_four" = 6L,
            "draw_two"       = 5L,
            "skip"           = 4L,
            "reverse"        = 3L,
            "wild"           = 2L,
            1L)  # number
        }, integer(1))
        return(legal_indices[which.max(priority)])
      }
      if (player$strategy == "colour_matcher") {
        # Prefer cards that keep the current colour (don't change it).
        cols <- vapply(legal_indices,
                       function(i) hand[[i]]$colour, character(1))
        keep <- which(cols == self$active_colour)
        if (length(keep) > 0L) return(legal_indices[keep[1]])
        # Otherwise random
        if (length(legal_indices) == 1L) return(legal_indices)
        return(sample(legal_indices, 1L))
      }
      stop("Unknown strategy: ", player$strategy)
    },

    pick_colour = function(player) {
      hand <- player$hand()
      if (length(hand) == 0L) return(sample(c("red","yellow","green","blue"), 1L))
      cols <- vapply(hand, function(c) c$colour, character(1))
      cols <- cols[cols != "black"]
      if (length(cols) == 0L) {
        return(sample(c("red","yellow","green","blue"), 1L))
      }
      tab <- table(cols)
      names(tab)[which.max(tab)]
    },

    # ---- Stacking helpers ----
    stackable_indices = function(player) {
      if (self$pending_draw == 0L) return(integer(0))
      top_type <- self$top_card()$type
      hand <- player$hand()
      if (length(hand) == 0L) return(integer(0))
      idx <- which(vapply(hand, function(c) {
        if (top_type == "draw_two") {
          c$type == "draw_two"   # only +2 stacks on +2 (simple house rule)
        } else if (top_type == "wild_draw_four") {
          c$type == "wild_draw_four"
        } else FALSE
      }, logical(1)))
      idx
    },

    take_pending_draw = function() {
      victim <- self$current_player()
      n_draw <- self$pending_draw
      for (i in seq_len(n_draw)) victim$receive(private$safe_draw())
      private$log_event(sprintf("%s draws %d (stacked).", victim$name, n_draw))
      self$pending_draw <- 0L
      private$advance_turn(1L)
      invisible(self)
    },

    resolve_pending_draw_play = function(card_idx, wild_colour) {
      p <- self$current_player()
      stackable <- private$stackable_indices(p)
      if (!card_idx %in% stackable) {
        stop("With pending draw stack, you can only play a stacking card or take the draw")
      }
      private$do_play(card_idx, wild_colour)
    },

    filter_w4_legality = function(player, legal) {
      if (length(legal) == 0L) return(legal)
      hand <- player$hand()
      keep <- vapply(legal, function(i) {
        c <- hand[[i]]
        if (c$type != "wild_draw_four") return(TRUE)
        !player$has_colour(self$active_colour)
      }, logical(1))
      legal[keep]
    }
  )
)

# Tiny null-coalescing operator
`%||%` <- function(a, b) if (is.null(a)) b else a
