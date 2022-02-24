globals <- new.env()

#' Browser base class
#'
#' Base class for browsers like Chrome, Chromium, etc. Defines the interface
#' used by various browser implementations. It can represent a local browser
#' process or one running remotely.
#'
#' The \code{initialize()} method of an implementation should set private$host
#' and private$port. If the process is local, the \code{initialize()} method
#' should also set private$process.
#'
#' @export
Browser <- R6Class("Browser",
  public = list(
    # Returns TRUE if the browser is running locally, FALSE if it's remote.
    is_local = function() !is.null(private$process),

    get_process = function() private$process,

    get_host = function() private$host,

    get_port = function() private$port,

    get_path = function() private$path,

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
    path = NULL,
    finalize = function(e) {
      if (self$is_local()) {
        self$close()
      }
    }
  )
)
