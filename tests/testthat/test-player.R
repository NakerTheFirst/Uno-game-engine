test_that("Player initialises correctly", {
  p <- Player$new("Alice", strategy = "random")
  expect_equal(p$name, "Alice")
  expect_equal(p$strategy, "random")
  expect_equal(p$hand_size(), 0L)
  expect_equal(p$score, 0L)
})

test_that("Player rejects invalid strategy", {
  expect_error(Player$new("Bob", strategy = "cheater"))
})

test_that("Player rejects empty name", {
  expect_error(Player$new(""))
})

test_that("receive() and hand_size() work", {
  p <- Player$new("Alice")
  card <- Card$new("red", value = 5L, type = "number")
  p$receive(card)
  expect_equal(p$hand_size(), 1L)
})

test_that("receive_many() adds multiple cards", {
  p <- Player$new("Alice")
  cards <- list(
    Card$new("red", value = 1L, type = "number"),
    Card$new("blue", type = "skip"),
    Card$new("black", type = "wild")
  )
  p$receive_many(cards)
  expect_equal(p$hand_size(), 3L)
})

test_that("remove_at() removes card at correct position", {
  p <- Player$new("Alice")
  c1 <- Card$new("red",  value = 1L, type = "number")
  c2 <- Card$new("blue", value = 2L, type = "number")
  p$receive(c1); p$receive(c2)
  removed <- p$remove_at(1L)
  expect_equal(removed$value, 1L)
  expect_equal(p$hand_size(), 1L)
})

test_that("remove_at() errors on out-of-bounds index", {
  p <- Player$new("Alice")
  p$receive(Card$new("red", value = 1L, type = "number"))
  expect_error(p$remove_at(5L))
})

test_that("legal_indices() returns matching card positions", {
  p   <- Player$new("Alice")
  top <- Card$new("red", value = 5L, type = "number")
  # Same colour – matches
  p$receive(Card$new("red", value = 3L, type = "number"))
  # Different colour & value – does not match
  p$receive(Card$new("blue", value = 2L, type = "number"))
  # Wild – always matches
  p$receive(Card$new("black", type = "wild"))

  idx <- p$legal_indices(top, "red")
  expect_setequal(idx, c(1L, 3L))
})

test_that("legal_indices() returns empty on empty hand", {
  p   <- Player$new("Alice")
  top <- Card$new("red", value = 5L, type = "number")
  expect_length(p$legal_indices(top, "red"), 0L)
})

test_that("has_colour() returns TRUE when colour is in hand", {
  p <- Player$new("Alice")
  p$receive(Card$new("green", value = 1L, type = "number"))
  expect_true(p$has_colour("green"))
  expect_false(p$has_colour("red"))
})

test_that("hand_points() sums correctly", {
  p <- Player$new("Alice")
  p$receive(Card$new("red",   value = 5L, type = "number"))   # 5 pts
  p$receive(Card$new("blue",  type  = "skip"))                 # 20 pts
  p$receive(Card$new("black", type  = "wild"))                 # 50 pts
  expect_equal(p$hand_points(), 75L)
})
