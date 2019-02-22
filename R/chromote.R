#' @importFrom websocket WebSocket
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @import promises later
#' @export
Chromote <- R6Class(
  "Chromote",
  lock_objects = FALSE,
  public = list(

    initialize = function(browser = default_browser()) {
      private$browser <- browser

      chrome_info <- fromJSON(private$url("/json"))

      private$command_callbacks <- new.env(parent = emptyenv())
      private$event_callbacks   <- new.env(parent = emptyenv())

      private$parent_loop <- current_loop()

      # Initialize the websocket with a private event loop
      with_private_loop({
        private$child_loop <- current_loop()

        private$ws <- WebSocket$new(
          chrome_info$webSocketDebuggerUrl,
          autoConnect = FALSE
        )

        private$ws$onMessage(private$on_message)

        p <- promise(function(resolve, reject) {
          private$ws$onOpen(resolve)

          # Allow up to 10 seconds to connect to browser.
          later(function() {
            reject(paste0("Chromote: timed out waiting for WebSocket connection to browser."))
          }, 10)
        })

        private$ws$connect()

        # Populate methods while the connection is being established.
        protocol <- jsonlite::fromJSON(private$url("/json/protocol"), simplifyVector = FALSE)
        proto <- process_protocol(protocol, self$.__enclos_env__)
        list2env(proto, self)

        private$schedule_child_loop()
        private$run_child_loop_until_resolved(p)
      })
    },

    view = function() {
      browseURL(private$url())
    },

    sync_mode = function(mode = NULL) {
      if (is.null(mode)) {
        return(private$sync_mode_)
      }
      if (! (identical(mode, TRUE) || identical(mode, FALSE))) {
        stop("mode must be TRUE or FALSE.")
      }

      private$sync_mode_ <- mode
    },

    default_timeout = 10
  ),
  private = list(
    browser = NULL,
    ws = NULL,

    # =========================================================================
    # Communication with browser and callbacks
    # =========================================================================
    last_msg_id = 0,
    command_callbacks = NULL,
    event_callbacks = NULL,

    send_command = function(msg, callback = NULL, timeout = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      p <- promise(function(resolve, reject) {
        private$ws$send(toJSON(msg, auto_unbox = TRUE))
        private$add_command_callback(msg$id, resolve)
      })

      if (!is.null(timeout) && !is.infinite(timeout)) {
        p <- promise_timeout(p, timeout, loop = private$child_loop,
          timeout_message = paste0("Chromote: timed out waiting for response to command ", msg$method)
        )
      }

      if (!is.null(callback)) {
        p <- p$then(callback)
      }

      # If synchronous mode, wait for the promise to resolve and return the
      # value. If async mode, return the promise immediately.
      if (private$sync_mode_) {
        return_value <- NULL
        p <- p$then(function(value) { return_value <<- value })
        # Error handling?
        private$run_child_loop_until_resolved(p)
        return(return_value)

      } else {
        return(invisible(p))
      }
    },

    add_command_callback = function(id, callback) {
      id <- as.character(id)
      private$command_callbacks[[id]] <- callback
    },

    invoke_command_callback = function(id, value) {
      id <- as.character(id)

      if (!exists(id, envir = private$command_callbacks, inherits = FALSE))
        return()

      callback <- private$command_callbacks[[id]]
      rm(list = id, envir = private$command_callbacks)
      callback(value)
    },


    register_event_listener = function(method_name, callback = NULL, timeout = NULL) {
      # Note: If callback is specified, then timeout is ignored
      if (!is.null(callback)) {
        deregister_callback_fn <- private$add_event_callback(method_name, callback, once = FALSE)
        return(invisible(deregister_callback_fn))
      }

      p <- promise(function(resolve, reject) {
        private$add_event_callback(method_name, resolve, once = TRUE)
      })

      if (!is.null(timeout) && !is.infinite(timeout)) {
        p <- promise_timeout(p, timeout, loop = private$child_loop,
          timeout_message = paste0("Chromote: timed out waiting for event ", method_name)
        )
      }

      # If synchronous mode, wait for the promise to resolve and return the
      # value. If async mode, return the promise immediately.
      if (private$sync_mode_) {
        return_value <- NULL
        p <- p$then(function(value) return_value <<- value)
        private$run_child_loop_until_resolved(p)
        return(return_value)

      } else {
        return(invisible(p))
      }
    },

    add_event_callback = function(event, callback, once) {
      if (is.null(private$event_callbacks[[event]])) {
        private$event_callbacks[[event]] <- Callbacks$new()
      }

      if (once) {
        orig_callback <- callback
        callback <- function(...) {
          tryCatch(
            orig_callback(...),
            finally = deregister_fn()
          )
        }
      }

      deregister_fn <- private$event_callbacks[[event]]$add(callback)
      deregister_fn
    },

    remove_event_callbacks = function(event) {
      # Removes ALL callbacks for a given event. In the future it might be
      # useful to implement finer control.
      private$event_callbacks[[event]] <- NULL
    },

    invoke_event_callbacks = function(event, params) {
      callbacks <- private$event_callbacks[[event]]
      if (is.null(callbacks) || callbacks$size() == 0)
        return()

      callbacks$invoke(params)
    },

    on_message = function(msg) {
      data <- fromJSON(msg$data, simplifyVector = FALSE)

      if (!is.null(data$method)) {
        # This is an event notification.
        #
        # The reason that the callback is wrapped in later() is to prevent a
        # possible race when a command response and an event notification arrive
        # in the same tick. See issue #1.
        later(function() {
          private$invoke_event_callbacks(data$method, data$params)
        })

      } else if (!is.null(data$id)) {
        # This is a response to a command.
        private$invoke_command_callback(data$id, data$result)
      }
    },

    # =========================================================================
    # Event loop for the websocket and the parent event loop
    # =========================================================================
    child_loop = NULL,
    parent_loop = NULL,
    sync_mode_ = TRUE,
    child_loop_is_scheduled = FALSE,

    schedule_child_loop = function() {
      # Make sure that if this function is called multiple times, there aren't
      # multiple streams of overlapping callbacks.
      if (private$child_loop_is_scheduled)
        return()

      # If the websocket has closed, there's no reason to run the child loop
      # anymore.
      if (private$ws$readyState() == 3) {
        return()
      }

      # This tells the parent loop to schedule one run of the child
      # (private) loop.
      later(private$run_child_loop, loop = private$parent_loop)

      private$child_loop_is_scheduled <- TRUE
    },

    run_child_loop = function() {
      private$child_loop_is_scheduled <- FALSE

      tryCatch(
        run_now(loop = private$child_loop),
        finally = {
          private$schedule_child_loop()
        }
      )
    },

    run_child_loop_until_resolved = function(p) {
      # Chain another promise that sets a flag when p is resolved.
      p_is_resolved <- FALSE
      p <- p$then(function(value) p_is_resolved <<- TRUE)

      err <- NULL
      p$catch(function(e) err <<- e)

      while (!p_is_resolved && is.null(err) && !loop_empty(loop = private$child_loop)) {
        run_now(loop = private$child_loop)
      }

      if (!is.null(err))
        stop(err)
    },

    # =========================================================================
    # Misc utility functions
    # =========================================================================
    url = function(path = NULL) {
      if (!is.null(path) && substr(path, 1, 1) != "/") {
        stop('path must be NULL or a string that starts with "/"')
      }
      paste0("http://", private$browser$get_host(), ":", private$browser$get_port(), path)
    }
  )
)
