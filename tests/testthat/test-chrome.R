expect_true_eventually <- function(expr, max_tries = 50, delay = 0.1) {
  expr <- enquo(expr)

  expect_true(
    with_retries(
      function() {
        if (!eval_tidy(expr)) {
          Sys.sleep(delay)
          stop(expr_text(expr), " is not yet TRUE")
        }
        TRUE
      },
      max_tries = max_tries
    )
  )
}

test_that("chrome with remote hosts", {
  skip_if_no_chromote()

  res <- with_random_port(function(port) {
    args <- c(
      get_chrome_args(),
      "--headless",
      "--remote-debugging-address=0.0.0.0",
      sprintf("--remote-debugging-port=%s", port)
    )

    p <- processx::process$new(find_chrome(), args)
    list(port = port, process = p)
  })

  withr::defer(if (!res$process$is_alive()) res$process$kill())

  remote <- ChromeRemote$new(host = "localhost", port = res$port)

  expect_true_eventually(remote$is_alive())
  expect_true(remote$close()) # does nothing but invisibly returns TRUE
  expect_true(remote$is_alive())

  chromote <- Chromote$new(browser = remote)
  expect_true(chromote$is_alive())
  expect_true(chromote$is_active())

  tab <- ChromoteSession$new(parent = chromote)
  expect_true(tab$is_active())

  # Close the websocket
  chromote$.__enclos_env__$private$ws$close()
  expect_true_eventually(!chromote$is_active())
  expect_true_eventually(!tab$is_active())

  # Reconnect
  tab2 <- suppressMessages(tab$respawn())
  expect_true_eventually(chromote$is_active())
  expect_true_eventually(tab2$is_active())

  tab2$close()
  expect_false(tab$is_active())
  tab2$parent$close()
  expect_true_eventually(!chromote$is_active())
  expect_true(chromote$is_alive()) # still alive, we haven't killed the process yet

  res$process$kill()
  expect_true_eventually(!chromote$is_alive())
})
