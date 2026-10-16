test_that("launch_chrome() stops port search when the OS cannot spawn Chrome", {
  attempts <- 0
  original <- rlang::catch_cnd(
    rlang::abort(
      "Cannot spawn process (system error 2)",
      class = "c_error_test"
    )
  )

  fake_process <- list(
    new = function(...) {
      attempts <<- attempts + 1
      rlang::cnd_signal(original)
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")

  err <- expect_error(
    launch_chrome("/path/to/missing-chrome", args = character()),
    class = "error_stop_port_search"
  )

  expect_equal(attempts, 1)
  expect_false(rlang::cnd_inherits(err, "error_no_available_port"))
  expect_identical(err$parent, original)
  expect_match(conditionMessage(err), "/path/to/missing-chrome", fixed = TRUE)
  expect_match(conditionMessage(err), "could not launch", fixed = TRUE)
})

test_that("launch_chrome() passes bare command names to processx unchanged", {
  commands <- character()
  fake_process <- list(
    new = function(command, ...) {
      commands <<- c(commands, command)
      stop("spawn failed")
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")

  expect_error(
    launch_chrome("chrome-not-on-path", args = character()),
    class = "error_stop_port_search"
  )
  expect_identical(commands, "chrome-not-on-path")
})

test_that("launch_chrome() stops after one spawn attempt with real processx", {
  # processx fails in `process$new()` before any process is created, so this
  # does not launch Chrome. The directory exists only for this test and the
  # executable inside it is absent.
  attempts <- 0
  real_process <- processx::process
  fake_process <- list(
    new = function(...) {
      attempts <<- attempts + 1
      real_process$new(...)
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")

  missing <- file.path(withr::local_tempdir(), "chrome")
  err <- expect_error(
    launch_chrome(missing, args = character()),
    class = "error_stop_port_search"
  )

  expect_equal(attempts, 1)
  expect_false(rlang::cnd_inherits(err, "error_no_available_port"))
  expect_false(is.null(err$parent))
  expect_match(conditionMessage(err), missing, fixed = TRUE)
})

test_that("launch_chrome() still retries other ports for retryable errors", {
  attempts <- 0
  log_dir <- withr::local_tempdir()
  fake_process <- list(
    new = function(...) {
      attempts <<- attempts + 1
      # Process starts but dies before exposing the debugging port, which is
      # a generic error that should be retried on a new port.
      error_file <- tempfile(tmpdir = log_dir)
      output_file <- tempfile(tmpdir = log_dir)
      file.create(c(error_file, output_file))
      list(
        is_alive = function() FALSE,
        get_error_file = function() error_file,
        get_output_file = function() output_file
      )
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")
  local_mocked_bindings(
    chrome_verify = function(...) list(status = 1),
    .package = "chromote"
  )
  withr::local_options(chromote.timeout = 1)

  expect_error(
    launch_chrome("/path/to/chrome", args = character()),
    class = "error_no_available_port"
  )
  expect_equal(attempts, 10)
})
