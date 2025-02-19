#' Local Chrome process
#'
#' @description
#' This is a subclass of [`Browser`] that represents a local browser. It extends
#' the [`Browser`] class with a [`processx::process`] object, which represents
#' the browser's system process.
#' @export
Chrome <- R6Class(
  "Chrome",
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

chrome_verify <- function(path = find_chrome()) {
  if (is_windows()) {
    return(chrome_verify_windows())
  }

  processx::run(
    command = path,
    args = c("--headless", "--version"),
    timeout = 2,
    error_on_status = FALSE
  )
}

chrome_verify_windows <- function(path = find_chrome()) {
  # Returns something similar to chrome_verify() for Windows, without actually
  # launching chrome, since `--version` doesn't work there.

  status <- function(code = 0, stdout = "", stderr = "") {
    list(status = code, stdout = stdout, stderr = stderr, timeout = FALSE)
  }

  path <- normalizePath(path)
  if (!file.exists(path)) {
    return(status(-1, stderr = sprintf("%s does not exist", path)))
  }

  has_powershell <- nzchar(Sys.which("powershell"))
  has_wmic <- nzchar(Sys.which("wmic"))
  status_unknown_version <- status(
    stdout = "Unknown (please manually verify the Chrome version)"
  )

  if (!has_powershell && !has_wmic) {
    return(status_unknown_version)
  }

  version <- ""

  if (has_powershell) {
    version <- chrome_windows_version_powershell(path)
  }

  if (!nzchar(version) && has_wmic) {
    version <- chrome_windows_version_wmic(path)
  }

  if (identical(version, "")) {
    return(status_unknown_version)
  }

  status(stdout = version)
}

chrome_windows_version_powershell <- function(path) {
  # Uses PowerShell to get the Chrome version
  command <- sprintf("(Get-Item \"%s\").VersionInfo.FileVersion", path)
  output <- system2(
    "powershell",
    c("-Command", shQuote(command)),
    stdout = TRUE
  )

  if (identical(output, "")) {
    return("")
  }

  output <- trimws(output)
  output[nzchar(output)][[1]]
}

chrome_windows_version_wmic <- function(path) {
  # Uses WMIC to get the Chrome version
  wmic_cmd <- sprintf(
    'wmic datafile where "name=\'%s\'" get version /value',
    gsub("\\\\", "\\\\\\\\", path)
  )

  output <- tryCatch(
    system(wmic_cmd, intern = TRUE),
    error = function(err) ""
  )

  if (identical(output, "")) {
    return("")
  }

  # Returns possibly several lines, one of which looks like
  # "Version=128.0.6613.85\r"
  output <- trimws(output)
  version <- grep("^Version=", output, value = TRUE)
  version <- sub("Version=", "", version)
  version <- paste(version, collapse = ", ") # might have more than one line

  return(version)
}

#' Show information about the chromote package and Chrome browser
#'
#' This function gathers information about the operating system, R version,
#' chromote package version, environment variables, Chrome path, and Chrome
#' arguments. It also verifies the Chrome installation and retrieves its version.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{os}{The operating system platform.}
#'   \item{version_r}{The version of R.}
#'   \item{version_chromote}{The version of the chromote package.}
#'   \item{envvar}{The value of the `CHROMOTE_CHROME` environment variable.}
#'   \item{path}{The path to the Chrome browser.}
#'   \item{args}{A vector of Chrome arguments.}
#'   \item{version}{The version of Chrome (if verification is successful).}
#'   \item{error}{The error message (if verification fails).}
#'   \item{.check}{A list with the status and output of the Chrome verification.}
#' }
#'
#' @examples
#' chromote_info()
#'
#' @export
chromote_info <- function() {
  pkg_version <- as.character(utils::packageVersion("chromote"))
  pkg_ref <- utils::packageDescription("chromote")$RemotePkgRef

  if (!is.null(pkg_ref) && !identical("chromote", pkg_ref)) {
    pkg_version <- sprintf("%s (%s)", pkg_version, pkg_ref)
  }

  info <- structure(
    list(
      os = as.character(R.version["platform"]),
      version_r = R.version.string,
      version_chromote = pkg_version,
      envvar = Sys.getenv("CHROMOTE_CHROME", ""),
      path = find_chrome(),
      args = c(chrome_headless_mode(), get_chrome_args())
    ),
    class = c("chromote_info", "list")
  )

  if (is.null(info$path)) {
    return(info)
  }

  info$.check <- chrome_verify(info$path)

  if (info$.check$status == 0) {
    info$version <- trimws(info$.check$stdout)
  } else {
    info$error <- info$.check$stderr
  }

  info
}

#' @export
print.chromote_info <- function(x, ...) {
  cat0 <- function(...) cat(..., "\n", sep = "")
  wrap <- function(x, nchar = 9) {
    x <- strwrap(x, width = getOption("width") - nchar, exdent = nchar)
    paste(x, collapse = "\n")
  }

  cat0("---- {chromote} ----")

  cat0("   System: ", x$os)
  cat0("R version: ", x$version_r)
  cat0(" chromote: ", x$version_chromote)

  cat0("\n---- Chrome ----")

  if (is.null(x$path)) {
    cat0(
      "Path: !! ",
      wrap("Could not find Chrome, is it installed on this system?")
    )
    cat0("      !! ", wrap("If yes, see `?find_chrome()` for help."))
    return(invisible(x))
  }

  cat0(
    "   Path: ",
    x$path,
    if (identical(x$path, x$envvar)) " (set by CHROMOTE_CHROME envvar)"
  )
  cat0("Version: ", x$version %||% "(unknown)")
  cat0("   Args: ", wrap(paste(x$args, collapse = " ")))
  if (x$.check$timeout) {
    cat0("  Error: Timed out.")
    cat0("  Error message:")
    cat0(x$error)
  } else if (!is.null(x$error)) {
    cat0("  Error: ", x$error)
  }
  invisible(x)
}

find_chrome_windows <- function() {
  tryCatch(
    {
      path <- utils::readRegistry(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\chrome.exe\\"
      )
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
    searched_for,
    " not found. ",
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
  env <- Sys.getenv("CHROMOTE_HEADLESS", "")
  env <- if (nzchar(env)) env else NULL

  # TODO Chrome v128 changed the default from --headless=old to --headless=new
  # in 2024-08. Old headless mode was effectively a separate browser render,
  # and while more performant did not share the same browser implementation as
  # headful Chrome. New headless mode will likely be useful to some, but in most
  # chromote use cases -- printing to PDF and testing -- we are not ready to
  # move to the new mode. Even once removed, the option may be useful if we
  # add support downloading specific versions of Chrome. (See rstudio/chromote#171)
  # 2025-01-16: Chrome v132 removed headless mode (rstudio/chromote#187)
  mode <- opt %||% env

  if (is.null(mode)) {
    return("--headless")
  }

  # Just pass headless along directly, Chrome will error if needed
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
  # Create temp locations for logs and crashes, grouped by chromote session
  tmp_session <- tempfile("chrome-", fileext = "%s")
  path_dir_crash <- sprintf(tmp_session, "-crashpad")
  path_stdout <- sprintf(tmp_session, "-stdout.log")
  path_stderr <- sprintf(tmp_session, "-stderr.log")

  p <- process$new(
    command = path,
    args = c(
      chrome_headless_mode(),
      paste0("--remote-debugging-port=", port),
      paste0("--remote-allow-origins=http://127.0.0.1:", port),
      paste0("--crash-dumps-dir=", path_dir_crash),
      args
    ),
    supervise = TRUE,
    stdout = path_stdout,
    stderr = path_stderr,
    echo_cmd = getOption("chromote.launch.echo_cmd", FALSE)
  )

  connected <- FALSE
  timeout <- getOption("chromote.timeout", 10)
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    if (!p$is_alive()) {
      error_logs_path <- p$get_error_file()
      error_logs <- paste(readLines(error_logs_path), collapse = "\n")
      stdout_file <- p$get_output_file()

      verify <- chrome_verify()

      stop(
        "Failed to start chrome. ",
        if (verify$status == 0) {
          "Chrome is available on your system, so this error may be a configuration issue. "
        } else {
          "Chrome does not appear to be runnable on your system. "
        },
        "Try `chromote_info()` to check and verify your settings. ",
        if (nzchar(error_logs)) {
          sprintf(
            "\nLog file: %s\nError:\n%s",
            error_logs_path,
            trimws(error_logs)
          )
        } else {
          "No error messages were logged."
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

        output_port <- sub(
          "^DevTools listening on ws://[0-9\\.]+:(\\d+)/.*",
          "\\1",
          output
        )
        output_port <- as.integer(output_port)
        if (is.na(output_port) || output_port != port) stop()

        con <- url(paste0("http://127.0.0.1:", port, "/json/protocol"), "rb")
        if (!isOpen(con)) break # Failed to connect

        connected <- TRUE
        close(con)
      },
      warning = function(e) {
      },
      error = function(e) {
      }
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
    port = port
  )
}

#' Remote Chrome process
#'
#' @description
#' Remote Chrome process
#'
#' @export
ChromeRemote <- R6Class(
  "ChromeRemote",
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
