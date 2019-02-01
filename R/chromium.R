globals <- new.env()

globals$process <- NULL
globals$port <- NULL

find_chromium <- function() {
  if (is_mac()) {
    "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
  } else {
    stop("Platform currently not supported")
  }
}

#' @importFrom processx process
ensure_browser_running <- function() {
  if (!is.null(globals$process) && globals$process$is_alive()) {
    return(list(
      process = globals$process,
      port = globals$port
    ))
  }

  exe <- find_chromium()

  host <- "127.0.0.1"
  port <- random_open_port()

  args <- c(
    "--headless",
    paste0("--remote-debugging-port=", port)
  )

  p <- process$new(
    command = exe, args = args, supervise = TRUE,
    stdout = tempfile("chromium-stdout-", fileext = ".log"),
    stderr = tempfile("chromium-stderr-", fileext = ".log")
  )

  if (!p$is_alive()) {
    stop(
      "Failed to start chromium. Error: ",
      strwrap(p$read_error_lines())
    )
  }

  # TODO: In the future, move this so it doesn't block?
  # Poll for the debugging port to be open.
  connected <- FALSE
  end <- Sys.time() + 10
  while (!connected && Sys.time() < end) {
    tryCatch(
      {
        con <- url(paste0("http://127.0.0.1:", port, "/json/protocol"), "rb")
        if (isOpen(con)) {
          connected <- TRUE
          close(con)
          break
        }
      },
      warning = function(e) {},
      error = function(e) {}
    )

    cat(".")
    Sys.sleep(0.2)
  }

  if (!connected) {
    stop("Chromium debugging port not open after 10 seconds.")
  }


  globals$process <- p
  globals$port <- port

  list(
    process = p,
    port = port
  )
}
