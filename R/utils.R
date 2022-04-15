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
