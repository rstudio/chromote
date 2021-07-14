

test_that("default args are retrieved", {
  expect_gte(length(default_chromote_args()), 6)
})

test_that("default args can be reset", {
  # safety
  on.exit({
    set_default_chromote_args(-1)
  }, add = TRUE)

  # Exists
  expect_gte(length(default_chromote_args()), 6)

  # Remove
  set_default_chromote_args(NULL)
  expect_equal(length(default_chromote_args()), 0)

  # Reset
  set_default_chromote_args(-1)
  expect_gte(length(default_chromote_args()), 6)

  # Remove
  set_default_chromote_args(character(0))
  expect_equal(length(default_chromote_args()), 0)

  # Reset
  set_default_chromote_args(-1)
  expect_gte(length(default_chromote_args()), 6)
})

test_that("default args can be overwritten", {
  # safety
  on.exit({
    set_default_chromote_args(-1)
  }, add = TRUE)

  expect_gte(length(default_chromote_args()), 6)
  set_default_chromote_args(c("hello", "goodbye"))

  expect_equal(length(default_chromote_args()), 2)
})

test_that("type checking", {
  # safety
  on.exit({
    set_default_chromote_args(-1)
  }, add = TRUE)

  expect_error(set_default_chromote_args(NA))
  expect_error(set_default_chromote_args(NaN))
  expect_error(set_default_chromote_args(1:10))
})
