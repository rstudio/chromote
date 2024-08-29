#' Local Chrome process
#'
#' @description
#' This is a subclass of [`Browser`] that represents a local browser. It extends
#' the [`Browser`] class with a [`processx::process`] object, which represents
#' the browser's system process.
#' @export
Chrome <- R6Class("Chrome",
  inherit = Browser,
  public = list(
    #' @description Create a new Chrome object.
    #' @param path Location of chrome installation
    #' @param args A character vector of command-line arguments passed when
    #'   initializing Chrome. Single on-off arguments are passed as single
    #'   values (e.g.`"--disable-gpu"`), arguments with a value are given with a
    #'   nested character vector (e.g. `c("--force-color-profile", "srgb")`).
    #'   See
    #'   [here](https://peter.sh/experiments/chromium-command-line-switches/)
    #'   for a list of possible arguments. Defaults to [`get_chrome_args()`].
    #' @return A new `Chrome` object.
    #' @seealso [`get_chrome_args()`]
    initialize = function(path = find_chrome(), args = get_chrome_args()) {
      if (is.null(path)) {
        stop("Invalid path to Chrome")
      }
      res <- launch_chrome(path, args)
      private$host <- "127.0.0.1"
      private$process <- res$process
      private$port <- res$port
      private$path <- path
    },
    #' @description Browser application path
    get_path = function() private$path
  ),
  private = list(
    path = NULL
  )
)

#' Find path to Chrome or Chromium browser
#'
#' @description
#' \pkg{chromote} requires a Chrome- or Chromium-based browser with support for
#' the Chrome DevTools Protocol. There are many such browser variants,
#' including [Google Chrome](https://www.google.com/chrome/),
#' [Chromium](https://www.chromium.org/chromium-projects/),
#' [Microsoft Edge](https://www.microsoft.com/en-us/edge) and others.
#'
#' If you want \pkg{chromote} to use a specific browser, set the
#' `CHROMOTE_CHROME` environment variable to the full path to the browser's
#' executable. Note that when `CHROMOTE_CHROME` is set, \pkg{chromote} will use
#' the value without any additional checks. On Mac, for example, one could use
#' Microsoft Edge by setting `CHROMOTE_CHROME` with the following:
#'
#' ```r
#' Sys.setenv(
#'   CHROMOTE_CHROME = "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
#' )
#' ```
#'
#' When `CHROMOTE_CHROME` is not set, `find_chrome()` will perform a limited
#' search to find a reasonable executable. On Windows, `find_chrome()` consults
#' the registry to find `chrome.exe`. On Mac, it looks for `Google Chrome` in
#' the `/Applications` folder (or tries the same checks as on Linux). On Linux,
#' it searches for several common executable names.
#'
#' @examples
#' find_chrome()
#'
#' @returns A character vector with the value of `CHROMOTE_CHROME`, or a path to
#'   the discovered Chrome executable. If no path to is found, `find_chrome()`
#'   returns `NULL`.
#'
#' @export
find_chrome <- function() {
  if (Sys.getenv("CHROMOTE_CHROME") != "") {
    return(Sys.getenv("CHROMOTE_CHROME"))
  }

  path <-
    if (is_mac()) {
      inform_if_chrome_not_found(find_chrome_mac())

    } else if (is_windows()) {
      inform_if_chrome_not_found(find_chrome_windows())

    } else if (is_linux() || is_openbsd()) {
      inform_if_chrome_not_found(
        find_chrome_linux(),
        searched_for = "`google-chrome`, `chromium-browser` and `chrome` were",
        extra_advice = "or adding one of these executables to your PATH"
      )

    } else {
      message("Platform currently not supported")
      NULL
    }

  path
}

find_chrome_windows <- function() {
  tryCatch(
    {
      path <- utils::readRegistry("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\")
      path[["(Default)"]]
    },
    error = function(e) {
      NULL
    }
  )
}

find_chrome_mac <- function() {
  path_default <- "/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
  if (file.exists(path_default)) {
    return(path_default)
  }

  find_chrome_linux()
}

find_chrome_linux <- function() {
  possible_names <- c(
    "google-chrome",
    "google-chrome-stable",
    "chromium-browser",
    "chromium",
    "google-chrome-beta",
    "google-chrome-unstable",
    "chrome"
  )

  for (path in possible_names) {
    path <- Sys.which(path)
    if (nzchar(path)) {
      return(path)
    }
  }

  NULL
}

inform_if_chrome_not_found <- function(
  path,
  searched_for = "Google Chrome was",
  extra_advice = ""
) {
  if (!is.null(path)) return(invisible(path))

  message(
    searched_for, " not found. ",
    "Try setting the `CHROMOTE_CHROME` environment variable to the executable ",
    "of a Chromium-based browser, such as Google Chrome, Chromium or Brave",
    if (nzchar(extra_advice)) " ",
    extra_advice,
    "."
  )

  NULL
}

chrome_headless_mode <- function() {
  opt <- getOption("chromote.headless", NULL)
  env <- Sys.getenv("CHROMOTE_HEADLESS", NULL)
  
  # TODO Chrome v128 changed the default from --headless=old to --headless=new
  # in 2024-08. Old headless mode was effectively a separate browser render,
  # and while more performant did not share the same browser implementation as
  # headful Chrome. New headless mode will likely be useful to some, but in most
  # chromote use cases -- printing to PDF and testing -- we are not ready to
  # move to the new mode. We'll use `--headless=old` as the default for now
  # until the new mode is more stable, or until we add support for downloading
  # specific versions of Chrome. (See rstudio/chromote#171)
  default_mode <- "old"
  mode <- tolower(opt %||% env %||% default_mode)

  if (!mode %in% c("old", "new")) {
    used <- if (!is.null(opt)) "chromote.headless" else "CHROMOTE_HEADLESS"
    rlang::inform(
      sprintf('Invalid value for `%s`. Using `"%s"`.', used, default_mode)
    )
    mode <- default_mode
  }

  sprintf("--headless=%s", mode)
}

launch_chrome <- function(path = find_chrome(), args = get_chrome_args()) {
  if (is.null(path)) {
    stop("Invalid path to Chrome")
  }

  res <- with_random_port(launch_chrome_impl, path = path, args = args)
  res
}

launch_chrome_impl <- function(path, args, port) {
  p <- process$new(
    command = path,
    args = c(
      chrome_headless_mode(),
      paste0("--remote-debugging-port=", port),
      paste0("--remote-allow-origins=http://127.0.0.1:", port),
      args
    ),
    supervise = TRUE,
    stdout = tempfile("chrome-stdout-", fileext = ".log"),
    stderr = tempfile("chrome-stderr-", fileext = ".log"),
    echo_cmd = getOption("chromote.launch.echo_cmd", FALSE)
  )

  connected <- FALSE
  timeout <- getOption("chromote.timeout", 10)
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    if (!p$is_alive()) {
      error_logs <- paste(readLines(p$get_error_file()), collapse = "\n")
      stdout_file <- p$get_output_file()
      
      stop(
        if (nzchar(error_logs)) {
          paste0("Failed to start chrome. Error:\n", error_logs)
        } else {
          "Failed to start chrome. (No error messages were printed.)"
        },
        if (file.info(stdout_file)$size > 0) {
          paste0(
            "\nThe following log file may contain more information:\n",
            stdout_file
          )
        }
      )
    }
    
    tryCatch(
      {
        # Find port number from output
        output <- readLines(p$get_error_file())
        output <- output[grepl("^DevTools listening on ws://", output)]
        if (length(output) != 1) stop() # Just break out of the tryCatch

        output_port <- sub("^DevTools listening on ws://[0-9\\.]+:(\\d+)/.*", "\\1", output)
        output_port <- as.integer(output_port)
        if (is.na(output_port) || output_port != port) stop()

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
    rlang::abort(
      paste("Chrome debugging port not open after", timeout, "seconds."),
      class = "error_stop_port_search"
    )
  }

  list(
    process = p,
    port    = port
  )
}


#' Remote Chrome process
#'
#' @description
#' Remote Chrome process
#'
#' @export
ChromeRemote <- R6Class("ChromeRemote",
  inherit = Browser,
  public = list(
    #' @description Create a new ChromeRemote object.
    #' @param host A string that is a valid IPv4 or IPv6 address. `"0.0.0.0"`
    #' represents all IPv4 addresses and `"::/0"` represents all IPv6 addresses.
    #' @param port A number or integer that indicates the server port.
    initialize = function(host, port) {
      private$host <- host
      private$port <- port
    }
  )
)
