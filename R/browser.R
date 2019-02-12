globals <- new.env()

#' @importFrom processx process

# Base class for browsers like Chrome, Chromium, etc. Defines the interface
# used by various browser implementations. The values of private$process and
# private$port should be set in an implementation's initialize() method.
Browser <- R6Class("Browser",
  public = list(
    get_process = function() private$process,

    get_port = function() private$port,

    is_alive = function() private$process$is_alive(),

    close = function() {
      if (private$process$is_alive()) {
        private$process$signal(tools::SIGTERM)
      }
    }
  ),
  private = list(
    process = NULL,
    port = NULL
  )
)
