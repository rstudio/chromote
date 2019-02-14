
# The data structure for storing callbacks is essentially a queue: items are
# added to the end, and removed from the front. Occasionally a callback will
# be removed from the middle of the queue. For each callback that's
# registered, we provide a function that can remove that callback from the
# queue.
Callbacks <- R6Class(
  "Callbacks",
  public = list(
    initialize = function() {
      # Use floating point because it has greater range than int while
      # maintaining precision of 1.0.
      private$nextId <- 1.0
      private$callbacks <- new.env(parent = emptyenv())
    },
    add = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function.")
      }

      id <- sprintf("%013.f", private$nextId)
      private$nextId <- private$nextId + 1.0
      private$callbacks[[id]] <- callback

      # Return function for unregistering the callback. Suppress warnings that
      # could occur if the callback has already been unregistered.
      invisible(function() {
        if (exists(id, envir = private$callbacks, inherits = FALSE))
          rm(list = id, envir = private$callbacks, inherits = FALSE)
      })
    },
    invoke = function(..., on_error = NULL) {
      # Ensure that calls are invoked in the order that they were registered
      keys <- sort(ls(private$callbacks))

      errors <- character()
      if (is.null(on_error)) {
        on_error <- function(e) {
          errors[length(errors) + 1] <<- e$message
        }
      }

      for (key in keys) {
        callback <- private$callbacks[[key]]
        tryCatch(callback(...), error = on_error)
      }

      if (length(errors) != 0) {
        warning(paste0(
          length(errors), " errors occurred while executing callbacks:\n  ",
          paste(errors, collapse = "\n  ")
        ))
      }
    },
    clear = function() {
      private$callbacks <- new.env(parent = emptyenv())
    },
    size = function() {
      length(ls(private$callbacks, sorted = FALSE))
    }
  ),
  private = list(
    nextId = NULL,
    callbacks = NULL
  )
)
