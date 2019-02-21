globals <- new.env()

#' @importFrom processx process

# Base class for browsers like Chrome, Chromium, etc. Defines the interface
# used by various browser implementations. It can represent a local browser
# process or one running remotely.
#
# The initialize() method of an implementation should set private$host and
# private$port. If the process is local, the initialize() method should also
# set private$process.
Browser <- R6Class("Browser",
  public = list(
    # Returns TRUE if the browser is running locally, FALSE if it's remote.
    is_local = function() !is.null(private$process),

    get_process = function() private$process,

    get_host = function() private$host,

    get_port = function() private$port,

    close = function() {
      if (self$is_local() && private$process$is_alive()) {
        private$process$signal(tools::SIGTERM)
      }
    }
  ),
  private = list(
    process = NULL,
    host = NULL,
    port = NULL,
    finalize = function(e) {
      if (self$is_local()) {
        self$close()
      }
    }
  )
)


#' Returns a default browser object
#'
#' This function first calls \code{getOption("chromote.browser")} and sees
#' if the value is a function; if so, it invokes that function and returns the
#' value. If the value not a function (or is not unset), this function calls
#' \code{\link{chrome}()} and returns the value.
#'
#' @export
default_browser <- function() {
  if (is.function(getOption("chromote.browser"))) {
    getOption("chromote.browser")()
  } else {
    chrome()
  }
}
