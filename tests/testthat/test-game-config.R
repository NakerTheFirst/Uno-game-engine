test_that("GameConfig creates with default values", {
  cfg <- methods::new("GameConfig")
  expect_equal(cfg@n_players, 4L)
  expect_equal(cfg@hand_size, 7L)
  expect_false(cfg@stacking)
  expect_true(cfg@force_play)
})

test_that("GameConfig accepts valid custom values", {
  cfg <- methods::new("GameConfig",
    n_players = 3L, hand_size = 5L,
    stacking = TRUE, force_play = FALSE, seed = 42L)
  expect_equal(cfg@n_players, 3L)
  expect_equal(cfg@hand_size, 5L)
  expect_true(cfg@stacking)
  expect_equal(cfg@seed, 42L)
})

test_that("GameConfig rejects n_players out of 2-4 range", {
  expect_error(methods::validObject(
    methods::new("GameConfig", n_players = 1L)))
  expect_error(methods::validObject(
    methods::new("GameConfig", n_players = 5L)))
})

test_that("GameConfig rejects hand_size out of 5-10 range", {
  expect_error(methods::validObject(
    methods::new("GameConfig", hand_size = 4L)))
  expect_error(methods::validObject(
    methods::new("GameConfig", hand_size = 11L)))
})

test_that("GameConfig boundary values are accepted", {
  expect_no_error(methods::validObject(
    methods::new("GameConfig", n_players = 2L, hand_size = 5L)))
  expect_no_error(methods::validObject(
    methods::new("GameConfig", n_players = 4L, hand_size = 10L)))
})
