make_result <- function(winner_idx = 1L,
                        names = c("Alice", "Bob"),
                        strats = c("random", "aggressive"),
                        sizes  = c(0L, 5L),
                        points = c(0L, 40L),
                        turns  = 30L) {
  new_game_result(
    winner       = names[winner_idx],
    winner_idx   = winner_idx,
    player_names = names,
    strategies   = strats,
    hand_sizes   = sizes,
    hand_points  = points,
    n_turns      = turns,
    log          = character(0)
  )
}

# ---- GameResult construction ----

test_that("new_game_result returns a GameResult", {
  res <- make_result()
  expect_s3_class(res, "GameResult")
})

test_that("new_game_result stores all fields correctly", {
  res <- make_result(winner_idx = 2L,
                     names  = c("Alice", "Bob"),
                     strats = c("random", "random"),
                     sizes  = c(3L, 0L),
                     points = c(25L, 0L),
                     turns  = 42L)
  expect_equal(res$winner, "Bob")
  expect_equal(res$winner_idx, 2L)
  expect_equal(res$n_turns, 42L)
  expect_equal(res$hand_sizes, c(3L, 0L))
})

test_that("new_game_result rejects mismatched vectors", {
  expect_error(new_game_result(
    winner = "Alice", winner_idx = 1L,
    player_names = c("Alice","Bob"),
    strategies   = c("random"),       # length mismatch
    hand_sizes   = c(0L, 5L),
    hand_points  = c(0L, 40L),
    n_turns      = 10L,
    log          = character(0)
  ))
})

# ---- print.GameResult ----

test_that("print.GameResult outputs winner line", {
  res <- make_result()
  out <- capture.output(print(res))
  expect_true(any(grepl("Alice", out)))
  expect_true(any(grepl("Winner", out)))
})

# ---- summary.GameResult ----

test_that("summary.GameResult returns a tibble", {
  res <- make_result()
  s   <- summary(res)
  expect_s3_class(s, "tbl_df")
  expect_true("player" %in% names(s))
  expect_true("is_winner" %in% names(s))
})

# ---- Leaderboard ----

test_that("new_leaderboard creates empty Leaderboard", {
  lb <- new_leaderboard()
  expect_s3_class(lb, "Leaderboard")
  expect_length(lb$games, 0L)
})

test_that("add_game_result adds a game", {
  lb <- new_leaderboard()
  lb <- add_game_result(lb, make_result())
  expect_length(lb$games, 1L)
})

test_that("update_leaderboard is an alias for add_game_result", {
  lb <- new_leaderboard()
  lb <- update_leaderboard(lb, make_result())
  expect_length(lb$games, 1L)
})

test_that("print.Leaderboard shows games played", {
  lb <- new_leaderboard()
  lb <- add_game_result(lb, make_result())
  lb <- add_game_result(lb, make_result(winner_idx = 2L,
                                        strats = c("random","aggressive")))
  out <- capture.output(print(lb))
  expect_true(any(grepl("2 games", out)))
})

test_that("summary.Leaderboard returns a tibble with win_rate", {
  lb <- new_leaderboard()
  lb <- add_game_result(lb, make_result())
  lb <- add_game_result(lb, make_result())
  s  <- summary(lb)
  expect_s3_class(s, "tbl_df")
  expect_true("win_rate" %in% names(s))
  # Alice won both games
  alice_row <- s[s$player == "Alice", ]
  expect_equal(alice_row$wins, 2L)
  expect_equal(alice_row$win_rate, 1.0)
})

test_that("summary.Leaderboard handles empty leaderboard", {
  lb <- new_leaderboard()
  s  <- summary(lb)
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 0L)
})
