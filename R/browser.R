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
    #' @description Is local browser?
    #' Returns TRUE if the browser is running locally, FALSE if it's remote.
    is_local = function() !is.null(private$process),

    #' @description Browser process
    get_process = function() private$process,

    #' @description Browser Host
    get_host = function() private$host,

    #' @description Browser port
    get_port = function() private$port,

    #' @description Close the browser
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
