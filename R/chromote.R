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
        self$wait_for(p)
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


    # This runs the child loop until the promise is resolved.
    wait_for = function(p) {
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

    screenshot = function(selector = "body", filename = "screenshot.png",
      show = interactive())
    {
      if (length(filename) == 0 && !show) {
        stop("Cannot have empty filename and show=FALSE")
      }

      hybrid_chain(
        self$DOM$getDocument(),
        function(value) {
          self$DOM$querySelector(value$root$nodeId, selector)
        },
        function(value) {
          if (value$nodeId == 0) {
            stop("Selector failed")
          }
          self$DOM$getBoxModel(value$nodeId)
        },
        function(value) {
          if (is.null(value)) {
            stop("Selector failed")
          }
          xmin <- value$model$margin[[1]]
          xmax <- value$model$margin[[3]]
          ymin <- value$model$margin[[2]]
          ymax <- value$model$margin[[6]]
          self$Page$captureScreenshot(clip = list(
            x = xmin,
            y = ymin,
            width  = xmax - xmin,
            height = ymax - ymin,
            scale = 1
          ))
        },
        function(value) {
          temp_output <- FALSE
          if (is.null(filename)) {
            temp_output <- TRUE
            filename <- tempfile("chromote-screenshot-", fileext = ".png")
            on.exit(unlink(filename))
          }

          writeBin(jsonlite::base64_dec(value$data), filename)
          if (show) {
            showimage::show_image(filename)
          }

          if (temp_output) {
            invisible()
          } else {
            invisible(filename)
          }
        },
        catch = function(err) {
          warning("An error occurred: ", err)
        }
      )
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

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      p <- promise(function(resolve, reject) {
        private$ws$send(toJSON(msg, auto_unbox = TRUE))
        private$add_command_callback(msg$id, resolve, reject)
      })

      p <- p$catch(function(e) {
        stop("code: ", e$code,
             "\n  message: ", e$message,
             if (!is.null(e$data)) paste0("\n  data: ", e$data)
        )
      })

      if (!is.null(timeout) && !is.infinite(timeout)) {
        p <- promise_timeout(p, timeout, loop = private$child_loop,
          timeout_message = paste0("Chromote: timed out waiting for response to command ", msg$method)
        )
      }

      if (!is.null(callback)) {
        p <- p$then(onFulfilled = callback, onRejected = error)
      }

      # If synchronous mode, wait for the promise to resolve and return the
      # value. If async mode, return the promise immediately.
      if (private$sync_mode_) {
        return_value <- NULL
        p <- p$then(function(value) { return_value <<- value })
        # Error handling?
        self$wait_for(p)
        return(return_value)

      } else {
        return(invisible(p))
      }
    },

    add_command_callback = function(id, callback, error) {
      id <- as.character(id)
      private$command_callbacks[[id]] <- list(
        callback = callback,
        error = error
      )
    },

    invoke_command_callback = function(id, value, error) {
      id <- as.character(id)

      if (!exists(id, envir = private$command_callbacks, inherits = FALSE))
        return()

      handlers <- private$command_callbacks[[id]]
      rm(list = id, envir = private$command_callbacks)

      if (!is.null(error)) {
        handlers$error(error)

      } else if (!is.null(value)) {
        handlers$callback(value)
      }
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
        self$wait_for(p)
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
        private$invoke_command_callback(data$id, data$result, data$error)

      } else {
        message("Don't know how to handle message: ", msg$data)
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
