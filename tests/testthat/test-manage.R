skip_on_cran()

test_that("with_chrome_version('system') works", {
  system_path <- find_chrome()
  skip_if_not(nzchar(system_path), "Chrome is not installed on this system.")

  fake_chromote_path <- tempfile("chrome")

  local_chromote_chrome(fake_chromote_path)
  expect_equal(find_chrome(), fake_chromote_path)

  expect_equal(
    with_chrome_version("system", find_chrome(), quiet = TRUE),
    system_path
  )
})

try_chromote_info <- function() {
  info <- chromote_info()
  if (!is.null(info$error)) {
    rlang::abort(c("Could not resolve full `chromote_info()`.", i = info$error))
  }

  info$path <- sub(normalizePath("~/"), "~", info$path)
  list(path = info$path, version = info$version)
}

test_that("with_chrome_version() manages Chromote object", {
  chrome_versions_add("128.0.6612.0", "chrome")
  chrome_versions_add("129.0.6668.100", "chrome-headless-shell")

  expect_closed <- function(chromote_obj) {
    max_wait <- Sys.time() + 15
    while (chromote_obj$is_alive() && Sys.time() < max_wait) {
      Sys.sleep(0.1)
    }
    if (Sys.time() >= max_wait) {
      warning("Waited the full 15 seconds for the process to close")
    }
    expect_false(chromote_obj$is_alive())
  }

  chromote_128 <- NULL

  # Another copy of chromote 128 that we start globally, should be unaffected
  chromote_128_global <- Chromote$new(
    browser = Chrome$new(path = chrome_versions_path("128.0.6612.0", "chrome"))
  )

  with_chrome_version("128.0.6612.0", {
    expect_equal(find_chrome(), chrome_versions_path("128.0.6612.0"))
    if (!has_chromote()) {
      skip(sprintf(
        "Skipping because Chrome failed to start (%s)",
        find_chrome()
      ))
    }
    chromote_128 <- default_chromote_object()
    chromote_129 <- NULL

    with_chrome_version("129.0.6668.100", binary = "chrome-headless-shell", {
      expect_equal(
        find_chrome(),
        chrome_versions_path("129.0.6668.100", "chrome-headless-shell")
      )
      if (!has_chromote()) {
        skip(sprintf(
          "Skipping because Chrome failed to start (%s)",
          find_chrome()
        ))
      }
      chromote_129 <- default_chromote_object()

      expect_true(chromote_129$is_alive())
      expect_equal(chromote_129$get_browser()$get_path(), find_chrome())
      expect_true(!identical(chromote_129, chromote_128))
    })

    expect_equal(default_chromote_object(), chromote_128)

    expect_closed(chromote_129)
    expect_true(chromote_128$is_alive())
  })

  expect_closed(chromote_128)

  # The global chromote 128 process is still running
  expect_true(chromote_128_global$is_alive())
  chromote_128_global$close()
  expect_closed(chromote_128_global)
})

test_that("with_chrome_version() works", {
  chrome_versions_add("128.0.6612.0", "chrome")

  expect_snapshot(
    with_chrome_version("128.0.6612.0", with_retries(try_chromote_info)),
    variant = guess_platform()
  )

  with_chrome_version("128.0.6612.0", {
    if (!has_chromote()) {
      skip(sprintf(
        "Skipping because Chrome failed to start (%s)",
        find_chrome()
      ))
    }

    b <- ChromoteSession$new()

    expect_match(
      b$Runtime$evaluate("navigator.appVersion")$result$value,
      "HeadlessChrome/128"
    )
  })
})
