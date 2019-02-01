is_windows <- function() .Platform$OS.type == "windows"

is_mac     <- function() Sys.info()[['sysname']] == 'Darwin'

is_linux   <- function() Sys.info()[['sysname']] == 'Linux'



random_open_port <- function(min = 3000, max = 10000, n = 20) {
  # Unsafe port list from shiny::runApp()
  valid_ports <- setdiff(min:max, c(3659, 4045, 6000, 6665:6669, 6697))

  # Try up to n ports
  for (port in sample(valid_ports, n)) {
    handle <- NULL

    # Check if port is open
    tryCatch(
      handle <- httpuv::startServer("127.0.0.1", port, list()),
      error = function(e) { }
    )
    if (!is.null(handle)) {
      httpuv::stopServer(handle)
      return(port)
    }
  }

  stop("Cannot find an available port")
}


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
