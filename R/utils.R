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

with_random_port <- function(
  .f,
  min = 1024L,
  max = 49151L,
  n = 5,
  host = "http://127.0.0.1"
) {
  valid_ports <- setdiff(seq.int(min, max), unsafe_ports)

  # Try up to n ports
  n <- min(n, length(valid_ports))
  ports <- sample(valid_ports, n)
  for (port in ports) {
    res <- NULL

    # Try to run `.f()` with the random port
    res <- tryCatch(
      .f(host = host, port = port),
      error = function(err) if (identical(port, ports[n])) err,
      error_timeout = identity,
      system_command_error = identity
    )

    if (rlang::cnd_inherits(res, "error")) {
      # A timeout error or trying all ports is unlikely to be port-related problem
      rlang::cnd_signal(res)
    }

    if (!is.null(res)) {
      return(res)
    }
  }

  rlang::abort(
    "Cannot find an available port. Please try again.",
    class = "chromote_error_no_available_port"
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
