test_that("Card creates valid number cards", {
  card <- Card$new("red", value = 5L, type = "number")
  expect_equal(card$colour, "red")
  expect_equal(card$value, 5L)
  expect_equal(card$type, "number")
})

test_that("Card creates valid action cards with NA value", {
  card <- Card$new("blue", type = "skip")
  expect_equal(card$type, "skip")
  expect_true(is.na(card$value))
})

test_that("Wild cards must have colour 'black'", {
  expect_error(Card$new("red", type = "wild"),
               regexp = "Wild cards must be created with colour = 'black'")
})

test_that("Non-wild cards cannot have colour 'black'", {
  expect_error(Card$new("black", value = 5L, type = "number"))
})

test_that("Invalid colour is rejected", {
  expect_error(Card$new("purple", value = 1L, type = "number"))
})

test_that("Invalid type is rejected", {
  expect_error(Card$new("red", value = 1L, type = "powerup"))
})

test_that("Number card value must be 0-9", {
  expect_error(Card$new("red", value = 10L, type = "number"))
  expect_no_error(Card$new("red", value = 0L, type = "number"))
  expect_no_error(Card$new("red", value = 9L, type = "number"))
})

test_that("is_wild() returns TRUE only for wilds", {
  wild  <- Card$new("black", type = "wild")
  w4    <- Card$new("black", type = "wild_draw_four")
  num   <- Card$new("red", value = 3L, type = "number")
  expect_true(wild$is_wild())
  expect_true(w4$is_wild())
  expect_false(num$is_wild())
})

test_that("is_action() returns TRUE for non-numbers", {
  skip <- Card$new("green", type = "skip")
  num  <- Card$new("green", value = 0L, type = "number")
  expect_true(skip$is_action())
  expect_false(num$is_action())
})

test_that("matches() — same colour", {
  top  <- Card$new("red", value = 5L, type = "number")
  card <- Card$new("red", value = 3L, type = "number")
  expect_true(card$matches(top, "red"))
})

test_that("matches() — same value, different colour", {
  top  <- Card$new("red",  value = 7L, type = "number")
  card <- Card$new("blue", value = 7L, type = "number")
  expect_true(card$matches(top, "red"))
})

test_that("matches() — same action type", {
  top  <- Card$new("red",  type = "skip")
  card <- Card$new("blue", type = "skip")
  expect_true(card$matches(top, "red"))
})

test_that("matches() — wild always matches", {
  top  <- Card$new("red", value = 5L, type = "number")
  wild <- Card$new("black", type = "wild")
  expect_true(wild$matches(top, "red"))
})

test_that("matches() — unrelated cards don't match", {
  top  <- Card$new("red",  value = 5L, type = "number")
  card <- Card$new("blue", value = 3L, type = "number")
  expect_false(card$matches(top, "red"))
})

test_that("label() returns a sensible string", {
  num  <- Card$new("red", value = 5L, type = "number")
  skip <- Card$new("green", type = "skip")
  wild <- Card$new("black", type = "wild")
  w4   <- Card$new("black", type = "wild_draw_four")

  expect_equal(num$label(),  "red 5")
  expect_equal(skip$label(), "green Skip")
  expect_equal(wild$label(), "Wild")
  expect_equal(w4$label(),   "Wild +4")
})
