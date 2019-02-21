globals$chrome <- NULL

#' Return a Chrome object
#'
#' This will start a Chrome process if necessary. If one is already running,
#' the object representing that process will be returned.
#'
#' @export
chrome <- function() {
  if (is.null(globals$chrome) || !globals$chrome$is_alive()) {
    globals$chrome <- Chrome$new()
  }

  globals$chrome
}


Chrome <- R6Class("Chrome",
  inherit = Browser,
  public = list(
    initialize = function(path = find_chrome()) {
      res <- launch_chrome(path)
      private$process <- res$process
      private$port <- res$port
    }
  )
)


find_chrome <- function() {
  if (is_mac()) {
    "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"

  } else if (is_windows()) {
    path <- NULL
    tryCatch(
      {
        path <- readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\")
        path <- path[["(Default)"]]
      },
      error = function(e) { }
    )
    path

  } else {
    stop("Platform currently not supported")
  }
}


launch_chrome <- function(path = find_chrome()) {
  p <- process$new(
    command = path,
    args = c("--headless", "--remote-debugging-port=0"),
    supervise = TRUE,
    stdout = tempfile("chrome-stdout-", fileext = ".log"),
    stderr = tempfile("chrome-stderr-", fileext = ".log")
  )

  if (!p$is_alive()) {
    stop(
      "Failed to start chrome. Error: ",
      strwrap(p$read_error_lines())
    )
  }

  connected <- FALSE
  end <- Sys.time() + 10
  while (!connected && Sys.time() < end) {
    tryCatch(
      {
        # Find port number from output
        output <- readLines(p$get_error_file())
        output <- output[grepl("^DevTools listening on ws://", output)]
        if (length(output) != 1) stop() # Just break out of the tryCatch

        port <- sub("^DevTools listening on ws://[0-9\\.]+:(\\d+)/.*", "\\1", output)
        port <- as.integer(port)
        if (is.na(port)) stop()

        con <- url(paste0("http://127.0.0.1:", port, "/json/protocol"), "rb")
        if (!isOpen(con)) break  # Failed to connect

        connected <- TRUE
        close(con)
      },
      warning = function(e) {},
      error = function(e) {}
    )

    Sys.sleep(0.1)
  }

  if (!connected) {
    stop("Chrome debugging port not open after 10 seconds.")
  }

  list(
    process = p,
    port    = port
  )
}
