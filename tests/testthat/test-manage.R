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

with_retries <- function(fn, max_tries = 3) {
  retry <- function(tried = 0) {
    tryCatch(
      {
        fn()
      },
      error = function(err) {
        tried <- tried + 1
        if (tried >= max_tries) {
          rlang::abort(
            sprintf("Failed after %s tries", tried),
            parent = err
          )
        } else {
          retry(tried)
        }
      }
    )
  }

  retry()
}

try_chromote_info <- function() {
  info <- chromote_info()
  if (!is.null(info$error)) {
    rlang::abort(c("Could not resolve full `chromote_info()`.", i = info$error))
  }
  info
}

test_that("with_chrome_version() works", {
  chrome_versions_add("128.0.6612.0", "chrome")

  expect_snapshot(
    with_chrome_version("128.0.6612.0", with_retries(try_chromote_info)),
    variant = guess_platform()
  )
})
