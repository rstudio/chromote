# =============================================================================
# System
# =============================================================================

is_windows <- function() .Platform$OS.type == "windows"

is_mac     <- function() Sys.info()[['sysname']] == 'Darwin'

is_linux   <- function() Sys.info()[['sysname']] == 'Linux'

# =============================================================================
# Vectors
# =============================================================================

get_key <- function(x, key, default = stop("Key not present")) {
  if (key %in% names(x)) {
    x[[key]]
  } else {
    default
  }
}

fetch_key_list <- function(x, key, default = stop("Key not present")) {
  lapply(x, get_key, key, default = default)
}

fetch_key_c <- function(x, key, default = stop("Key not present")) {
  vapply(x, get_key, key, default = default, FUN.VALUE = "")
}

fetch_key_n <- function(x, key, default = stop("Key not present")) {
  vapply(x, get_key, key, default = default, FUN.VALUE = 0.0)
}

fetch_key_i <- function(x, key, default = stop("Key not present")) {
  vapply(x, get_key, key, default = default, FUN.VALUE = 0L)
}

fetch_key_l <- function(x, key, default = stop("Key not present")) {
  vapply(x, get_key, key, default = default, FUN.VALUE = FALSE)
}


drop_nulls <- function(x) {
  x[!vapply(x, is.null, TRUE)]
}


# =============================================================================
# Text
# =============================================================================

truncate <- function(x, n = 1000, message = "[truncated]") {
  if (length(x) != 1) {
    stop("Input must be a single string")
  }
  if (nchar(x) > n) {
    x <- paste0(substr(x, 1, n - nchar(message)), message)
  }
  x
}


# =============================================================================
# Protocol-related stuff
# =============================================================================

# Given an event name, return the domain: "Page.loadEventFired" -> "Page"
find_domain <- function(event) {
  sub("\\.[^.]+", "", event)
}


# =============================================================================
# Browser
# =============================================================================

# Force url to be opened by Chromium browser
browse_url <- function(path, chromote) {
  url <- chromote$url(path)

  browser <- chromote$get_browser()
  if (inherits(browser, "Chrome")) {
    # If locally available, use the local browser
    browser_path <- browser$get_path()
    # Quote the path if using a non-windows machine
    if (!is_windows()) browser_path <- shQuote(browser_path)
    utils::browseURL(url, browser_path)
  } else {
    # Otherwise pray opening the url works as expected
    # Users can set `options(browser=)` to override default behavior
    utils::browseURL(url)
  }
}


# =============================================================================
# Random Ports
# =============================================================================
#
# Borrowed from https://github.com/rstudio/httpuv/blob/main/R/random_port.R

#' Startup a service that requires a random port
#'
#' `with_random_port()` provides `startup()` with a random port value and runs
#' the function:
#'
#' 1. `startup()` always returns a value if successful.
#' 2. If `startup()` fails with a generic error, we assume the port is occupied
#'    and try the next random port.
#' 3. If `startup()` fails with an error classed with errors in `stop_on`,
#'    (`error_stop_port_search` or `system_command_error` by default), we stop
#'    the port search and rethrow the fatal error.
#' 4. If we try `n` random ports, the port search stops with an informative
#'    error that includes the last port attempt error.
#'
#' @param startup A function that takes a `port` argument, where `port` will be
#'   randomly selected. When successful, `startup()` should return a non-NULL
#'   value that will also be returned from `with_random_port()`. Generic errors
#'   emitted by this function are silently ignored: when `startup()` fails, we
#'   assume the port was unavailable and we try with a new port. Errors with the
#'   classes in `stop_on` fail immediately.
#' @param ... Additional arguments passed to `startup()`.
#' @param min,max Port range
#' @param n Maximum number of ports to try
#' @param stop_on Error classes that signal the port search should be stopped
#'
#' @return The result of `startup()`, or an error if `startup()` fails.
#' @noRd
with_random_port <- function(
  startup,
  ...,
  min = 1024L,
  max = 49151L,
  n = 10,
  stop_on = c("error_stop_port_search", "system_command_error")
) {
  stopifnot(is.function(startup))
  valid_ports <- setdiff(seq.int(min, max), unsafe_ports)

  # Try up to n ports
  n <- min(n, length(valid_ports))
  ports <- sample(valid_ports, n)
  err_port <- NULL

  for (port in ports) {
    success <- FALSE
    res <- NULL
    err_fatal <- NULL

    # Try to run `startup` with the random port
    tryCatch({
        res <- startup(port = port, ...)
        success <- TRUE
      },
      error = function(cnd) {
        if (rlang::cnd_inherits(cnd, stop_on)) {
          # Non generic errors that signal we should stop trying new ports
          err_fatal <<- cnd
          return()
        }
        # For other errors, they are probably because the port is already in
        # use. Don't do anything; we'll just continue in the loop, but we save
        # the last port retry error to throw in case it's informative.
        err_port <<- cnd
        NULL
      }
    )

    if (!is.null(err_fatal)) {
      rlang::cnd_signal(err_fatal)
    }

    if (isTRUE(success)) {
      return(res)
    }
  }

  rlang::abort(
    "Cannot find an available port. Please try again.",
    class = "error_no_available_port",
    parent = err_port
  )
}

# Ports that are considered unsafe by Chrome
# http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
# https://github.com/rstudio/shiny/issues/1784
unsafe_ports <- c(
  1,
  7,
  9,
  11,
  13,
  15,
  17,
  19,
  20,
  21,
  22,
  23,
  25,
  37,
  42,
  43,
  53,
  77,
  79,
  87,
  95,
  101,
  102,
  103,
  104,
  109,
  110,
  111,
  113,
  115,
  117,
  119,
  123,
  135,
  139,
  143,
  179,
  389,
  427,
  465,
  512,
  513,
  514,
  515,
  526,
  530,
  531,
  532,
  540,
  548,
  556,
  563,
  587,
  601,
  636,
  993,
  995,
  2049,
  3659,
  4045,
  6000,
  6665,
  6666,
  6667,
  6668,
  6669,
  6697
)
