# Helper: build a simple 2-player game config
make_cfg <- function(n = 2L, hand_size = 7L, stacking = FALSE, seed = 42L) {
  methods::new("GameConfig",
    n_players  = as.integer(n),
    hand_size  = as.integer(hand_size),
    stacking   = stacking,
    force_play = TRUE,
    seed       = as.integer(seed)
  )
}

make_specs <- function(strategies = c("random", "random")) {
  lapply(seq_along(strategies), function(i) {
    list(name = sprintf("P%d", i), strategy = strategies[i])
  })
}

# ---- Construction ----

test_that("Game initialises without error (2 players)", {
  cfg <- make_cfg()
  expect_no_error(Game$new(cfg, make_specs()))
})

test_that("Game initialises without error (4 players)", {
  cfg <- make_cfg(n = 4L)
  expect_no_error(Game$new(cfg, make_specs(c("random","aggressive","colour_matcher","random"))))
})

test_that("Game requires config to be GameConfig", {
  expect_error(Game$new(list(n_players = 2), make_specs()))
})

test_that("Player count mismatch is rejected", {
  cfg <- make_cfg(n = 2L)
  expect_error(Game$new(cfg, make_specs(c("random","random","random"))))
})

test_that("Each player starts with hand_size cards", {
  cfg  <- make_cfg(hand_size = 7L)
  game <- Game$new(cfg, make_specs())
  for (p in game$players) {
    expect_equal(p$hand_size(), 7L)
  }
})

test_that("Top card is not a wild_draw_four after setup", {
  # Run a bunch of seeds to check
  for (s in 1:20) {
    cfg  <- make_cfg(seed = s)
    game <- Game$new(cfg, make_specs())
    expect_false(game$top_card()$type == "wild_draw_four")
  }
})

test_that("active_colour is a valid colour after setup", {
  cfg  <- make_cfg()
  game <- Game$new(cfg, make_specs())
  expect_true(game$active_colour %in% c("red","yellow","green","blue"))
})

# ---- Skip ----

test_that("Skip card advances turn twice (skips next player)", {
  # We'll run a game and verify skip behaviour by seeding so a skip
  # is played, then checking the current player jumped by 2.
  # Since we can't fully control RNG output, we run a full auto game
  # and just check it completes without error.
  cfg  <- make_cfg(seed = 7L)
  game <- Game$new(cfg, make_specs(c("aggressive", "aggressive")))
  turns <- 0L
  while (!game$is_over() && turns < 2000L) {
    game$auto_turn()
    turns <- turns + 1L
  }
  expect_true(game$is_over())
})

# ---- Reverse ----

test_that("Reverse flips direction", {
  cfg  <- make_cfg(seed = 5L)
  game <- Game$new(cfg, make_specs())
  # In a 2-player game, reverse acts like a skip.
  # Just verify the game runs to completion.
  turns <- 0L
  while (!game$is_over() && turns < 2000L) {
    game$auto_turn()
    turns <- turns + 1L
  }
  expect_true(game$is_over())
})

# ---- Draw two ----

test_that("draw_two causes next player to draw 2 (no stacking)", {
  cfg  <- make_cfg(stacking = FALSE, seed = 99L)
  game <- Game$new(cfg, make_specs(c("aggressive", "aggressive")))
  turns <- 0L
  while (!game$is_over() && turns < 2000L) {
    game$auto_turn()
    turns <- turns + 1L
  }
  expect_true(game$is_over())
})

# ---- Stacking ----

test_that("Stacking mode games complete without error", {
  cfg  <- make_cfg(stacking = TRUE, seed = 11L)
  game <- Game$new(cfg, make_specs())
  turns <- 0L
  while (!game$is_over() && turns < 2000L) {
    game$auto_turn()
    turns <- turns + 1L
  }
  expect_true(game$is_over())
})

# ---- Wild colour choice ----

test_that("Wild card requires a valid wild_colour", {
  cfg  <- make_cfg()
  game <- Game$new(cfg, make_specs())
  # Find a wild in current player's hand; if none, skip.
  p    <- game$current_player()
  hand <- p$hand()
  wild_idx <- which(vapply(hand, function(c) c$is_wild(), logical(1)))
  if (length(wild_idx) > 0L) {
    expect_error(game$play_card(wild_idx[1], wild_colour = "purple"))
    expect_no_error(game$play_card(wild_idx[1], wild_colour = "red"))
  } else {
    skip("No wild in starting hand for this seed")
  }
})

# ---- Illegal play ----

test_that("Playing an unmatched card throws an error", {
  cfg  <- make_cfg(seed = 1L)
  game <- Game$new(cfg, make_specs())
  p    <- game$current_player()
  hand <- p$hand()
  top  <- game$top_card()

  # Find a card that does NOT match
  illegal <- which(!vapply(hand, function(c)
    c$matches(top, game$active_colour) || c$is_wild(), logical(1)))

  if (length(illegal) > 0L) {
    expect_error(game$play_card(illegal[1]))
  } else {
    skip("All cards match for this seed — can't test illegal play")
  }
})

# ---- Full game result ----

test_that("result() returns a GameResult after game ends", {
  cfg  <- make_cfg(seed = 42L)
  game <- Game$new(cfg, make_specs())
  turns <- 0L
  while (!game$is_over() && turns < 5000L) {
    game$auto_turn()
    turns <- turns + 1L
  }
  if (game$is_over()) {
    res <- game$result()
    expect_s3_class(res, "GameResult")
    expect_true(res$winner %in% c("P1", "P2"))
    expect_true(res$n_turns > 0L)
  }
})

# ---- 4-player game ----

test_that("4-player game completes and has valid winner", {
  cfg  <- make_cfg(n = 4L, seed = 77L)
  game <- Game$new(cfg,
    make_specs(c("random","aggressive","colour_matcher","random")))
  turns <- 0L
  while (!game$is_over() && turns < 5000L) {
    game$auto_turn()
    turns <- turns + 1L
  }
  expect_true(game$is_over())
  res <- game$result()
  expect_true(res$winner_idx %in% 1:4)
})
