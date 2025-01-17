min_chrome_arg_length <- 3 + is_inside_ci() + is_windows()

test_that("default args are retrieved", {
  expect_gte(length(default_chrome_args()), min_chrome_arg_length)
})

test_that("default args can be reset", {
  # safety
  cur_args <- get_chrome_args()
  on.exit(
    {
      set_chrome_args(cur_args)
    },
    add = TRUE
  )

  reset_chrome_args()

  # Exists
  expect_gte(length(get_chrome_args()), min_chrome_arg_length)

  # Remove
  set_chrome_args(NULL)
  expect_equal(length(get_chrome_args()), 0)
  expect_gte(length(default_chrome_args()), min_chrome_arg_length)

  # Reset
  reset_chrome_args()
  expect_gte(length(get_chrome_args()), min_chrome_arg_length)

  # Remove
  set_chrome_args(character(0))
  expect_equal(length(get_chrome_args()), 0)
})

test_that("default args can be overwritten", {
  # safety
  cur_args <- get_chrome_args()
  on.exit(
    {
      set_chrome_args(cur_args)
    },
    add = TRUE
  )

  reset_chrome_args()

  expect_gte(length(get_chrome_args()), min_chrome_arg_length)

  set_chrome_args(c("hello", "goodbye"))
  expect_equal(length(get_chrome_args()), 2)
})

test_that("type checking", {
  # safety
  cur_args <- get_chrome_args()
  on.exit(
    {
      set_chrome_args(cur_args)
    },
    add = TRUE
  )

  expect_error(set_chrome_args(NA))
  expect_error(set_chrome_args(NaN))
  expect_error(set_chrome_args(1:10))
})
