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

  globals$process <- process$new(
    command = exe,
    args = c("--headless", "--remote-debugging-port=0"),
    supervise = TRUE,
    stdout = tempfile("chromium-stdout-", fileext = ".log"),
    stderr = tempfile("chromium-stderr-", fileext = ".log")
  )

  if (!globals$process$is_alive()) {
    stop(
      "Failed to start chromium. Error: ",
      strwrap(p$read_error_lines())
    )
  }

  connected <- FALSE
  end <- Sys.time() + 10
  while (!connected && Sys.time() < end) {
    tryCatch(
      {
        # Find port number from output
        output <- readLines(globals$process$get_error_file())
        output <- output[grepl("^DevTools listening on ws://", output)]
        if (length(output) != 1) stop() # Just break out of the tryCatch

        port <- sub("^DevTools listening on ws://[0-9\\.]+:(\\d+)/.*", "\\1", output)
        port <- as.integer(port)
        if (is.na(port)) stop()

        con <- url(paste0("http://127.0.0.1:", port, "/json/protocol"), "rb")
        if (!isOpen(con)) stop()

        connected <- TRUE
        close(con)
      },
      warning = function(e) {},
      error = function(e) {}
    )

    Sys.sleep(0.1)
  }

  if (!connected) {
    stop("Chromium debugging port not open after 10 seconds.")
  }

  globals$port <- port

  list(
    process = globals$process,
    port = globals$port
  )
}
