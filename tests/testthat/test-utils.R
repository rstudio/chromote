test_that("with_random_port() tries expected number of ports in range", {
  min <- 2000L
  max <- 4000L
  n <- 25

  tried_ports <- c()

  try_unavailable_port <- function(port) {
    tried_ports <<- c(tried_ports, port)
    stop("Port ", port, " is unavailable.")
  }

  expect_error(
    with_random_port(
      try_unavailable_port,
      min = min,
      max = max,
      n = n
    )
  )

  expect_length(tried_ports, n)
  expect_true(all(tried_ports >= min))
  expect_true(all(tried_ports <= max))
})

test_that("with_random_port() stops trying for `error_stop_port_search` errors", {
  tried_ports <- c()

  try_port_with_fatal_error <- function(port) {
    tried_ports <<- c(tried_ports, port)
    rlang::abort(
      paste0("Port ", port, " is unavailable."),
      class = "error_stop_port_search"
    )
  }

  expect_error(
    with_random_port(try_port_with_fatal_error),
    class = "error_stop_port_search"
  )
  expect_length(tried_ports, 1)
})

test_that("with_random_port() returns result of `startup()`", {
  tried_ports <- c()

  accept_round_port <- function(port) {
    if (port %% 5 == 0) {
      return(port)
    }

    tried_ports <<- c(tried_ports, port)
    stop("Odd port")
  }

  port <- with_random_port(accept_round_port, n = 100)

  expect_true(port %% 5 == 0)

  if (length(tried_ports)) {
    expect_true(all(tried_ports %% 5 > 0))
  }
})

test_that("with_random_port() startup function can return NULL", {
  accept_any_port <- function(port) {
    NULL
  }

  port <- with_random_port(accept_any_port)
  expect_null(port)
})
