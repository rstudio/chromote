globals <- new.env()

#' Browser base class
#'
#' @description
#' Base class for browsers like Chrome, Chromium, etc. Defines the interface
#' used by various browser implementations. It can represent a local browser
#' process or one running remotely.
#'
#' @details
#' The `initialize()` method of an implementation should set `private$host`
#' and `private$port`. If the process is local, the `initialize()` method
#' should also set `private$process`.
#'
#' @export
Browser <- R6Class(
  "Browser",
  public = list(
    # Returns TRUE if the browser is running locally, FALSE if it's remote.
    #' @description Is local browser?
    #' Returns TRUE if the browser is running locally, FALSE if it's remote.
    is_local = function() !is.null(private$process),

    #' @description Browser process
    get_process = function() private$process,

    #' @description Is the process alive?
    is_alive = function() private$process$is_alive(),

    #' @description Browser Host
    get_host = function() private$host,

    #' @description Browser port
    get_port = function() private$port,

    #' @description Close the browser
    #' @param wait If an integer, waits a number of seconds for the process to
    #'   exit, killing the process if it takes longer than `wait` seconds to
    #'   close. Use `wait = TRUE` to wait for 10 seconds.
    close = function(wait = FALSE) {
      if (!self$is_local()) return(invisible())
      if (!private$process$is_alive()) return(invisible())

      if (!isFALSE(wait)) {
        if (isTRUE(wait)) wait <- 10
        check_number_whole(wait, min = 0)
      }

      private$process$signal(tools::SIGTERM)

      if (!isFALSE(wait)) {
        tryCatch(
          {
            private$process$wait(timeout = wait * 1000)
            if (private$process$is_alive()) {
              stop("shut it down") # ignored, used to escalate
            }
          },
          error = function(err) {
            # Still alive after wait...
            try(private$process$kill(), silent = TRUE)
          }
        )
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
