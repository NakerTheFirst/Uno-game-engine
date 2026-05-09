test_that("Deck has exactly 108 cards", {
  deck <- Deck$new()
  expect_equal(deck$size(), 108L)
})

test_that("Deck contains correct card composition", {
  deck  <- Deck$new()
  cards <- deck$cards()

  colours <- vapply(cards, function(c) c$colour, character(1))
  types   <- vapply(cards, function(c) c$type,   character(1))

  # 4 wilds + 4 wild_draw_fours
  expect_equal(sum(types == "wild"),           4L)
  expect_equal(sum(types == "wild_draw_four"), 4L)

  for (col in c("red", "yellow", "green", "blue")) {
    col_cards <- cards[colours == col]
    col_types <- vapply(col_cards, function(c) c$type, character(1))
    col_vals  <- vapply(col_cards, function(c) c$value %||% NA_integer_, integer(1))

    expect_equal(length(col_cards), 25L,
                 label = sprintf("colour %s must have 25 cards", col))
    expect_equal(sum(col_vals == 0L, na.rm = TRUE), 1L,
                 label = sprintf("%s must have exactly one 0", col))
    expect_equal(sum(col_types == "skip"),    2L)
    expect_equal(sum(col_types == "reverse"), 2L)
    expect_equal(sum(col_types == "draw_two"), 2L)
  }
})

test_that("draw() removes and returns a card", {
  deck <- Deck$new()
  size_before <- deck$size()
  card <- deck$draw()
  expect_s3_class(card, "Card")
  expect_equal(deck$size(), size_before - 1L)
})

test_that("draw_n() draws n cards", {
  deck  <- Deck$new()
  cards <- deck$draw_n(7L)
  expect_length(cards, 7L)
  expect_equal(deck$size(), 101L)
  expect_true(all(vapply(cards, inherits, logical(1), "Card")))
})

test_that("draw() reshuffles from discard when pile is empty", {
  deck <- Deck$new()
  # Draw all 108 cards
  all_cards <- deck$draw_n(108L)
  expect_equal(deck$size(), 0L)

  # Provide the first 107 as reshuffle material (keep last as top discard)
  reshuffle <- all_cards[1:107]
  card <- deck$draw(reshuffle_from = reshuffle)
  expect_s3_class(card, "Card")
})

test_that("draw() errors when empty and no reshuffle provided", {
  deck <- Deck$new()
  deck$draw_n(108L)
  expect_error(deck$draw(), regexp = "empty")
})

test_that("set_cards() replaces the pile", {
  deck  <- Deck$new()
  deck2 <- Deck$new()
  three <- deck2$draw_n(3L)
  deck$set_cards(three)
  expect_equal(deck$size(), 3L)
})

test_that("shuffle() doesn't change deck size", {
  deck <- Deck$new()
  deck$shuffle()
  expect_equal(deck$size(), 108L)
})
