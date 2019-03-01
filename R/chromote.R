#' @importFrom websocket WebSocket
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @import promises later
#' @export
Chromote <- R6Class(
  "Chromote",
  lock_objects = FALSE,
  public = list(

    initialize = function(browser = default_browser(), auto_events = TRUE) {
      private$browser <- browser
      private$auto_events <- auto_events

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
        protocol_objs <- process_protocol(protocol, self$.__enclos_env__)
        list2env(protocol_objs, self)

        # Find out which domains require the <domain>.enable command to enable
        # event notifications.
        private$event_enable_domains <- lapply(protocol_objs, function(domain) {
          is.function(domain$enable)
        })


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

    screenshot = function(selector = "body",
      filename = "screenshot.png",
      region = c("content", "padding", "border", "margin"),
      scale = 1,
      show = interactive())
    {
      region = match.arg(region)
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
          xmin <- value$model[[region]][[1]]
          xmax <- value$model[[region]][[3]]
          ymin <- value$model[[region]][[2]]
          ymax <- value$model[[region]][[6]]
          self$Page$captureScreenshot(clip = list(
            x = xmin,
            y = ymin,
            width  = xmax - xmin,
            height = ymax - ymin,
            scale = scale
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

    # Enable or disable message debugging. If enabled, R will print out the
    # JSON messages that are sent and received. If called with no value, this
    # method will print out the current debugging state.
    debug_messages = function(value = NULL) {
      if (is.null(value))
        return(private$debug_messages_)

      if (!(identical(value, TRUE) || identical(value, FALSE)))
        stop("value must be TRUE or FALSE")

      private$debug_messages_ <- value
    },

    default_timeout = 10
  ),

  private = list(
    browser = NULL,
    ws = NULL,

    # =========================================================================
    # Browser commands
    # =========================================================================
    last_msg_id = 0,
    command_callbacks = NULL,

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      p <- promise(function(resolve, reject) {
        msg_json <- toJSON(msg, auto_unbox = TRUE)
        private$ws$send(msg_json)
        private$debug_log("SEND ", msg_json)
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


    # =========================================================================
    # Browser events
    # =========================================================================
    event_callbacks = NULL,
    # For keeping count of the number of callbacks for each domain; if
    # auto_events is TRUE, then when the count goes from 0 to 1 or 1 to 0 for
    # a given domain, it will automatically enable or disable events for that
    # domain.
    event_callback_counts = list(),
    auto_events = NULL,

    # Some domains require a <domain>.event command to enable event
    # notifications, others do not. (Not really sure why.)
    event_enable_domains = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      domain <- find_domain(event)

      # Note: If callback is specified, then timeout is ignored
      if (!is.null(callback)) {
        deregister_callback_fn <- private$add_event_callback(event, callback, once = FALSE)
        return(invisible(deregister_callback_fn))
      }

      deregister_callback_fn <- NULL
      p <- promise(function(resolve, reject) {
        deregister_callback_fn <<- private$add_event_callback(event, resolve, once = TRUE)
      })

      if (!is.null(timeout) && !is.infinite(timeout)) {
        p <- promise_timeout(p, timeout, loop = private$child_loop,
          timeout_message = paste0("Chromote: timed out waiting for event ", event)
        )
      }

      # If synchronous mode, wait for the promise to resolve and return the
      # value. If async mode, return the promise immediately.
      if (private$sync_mode_) {
        on.exit(deregister_callback_fn(), add = TRUE)

        return_value <- NULL
        p <- p$then(function(value) return_value <<- value)
        self$wait_for(p)
        return(return_value)

      } else {
        p <- p$finally(deregister_callback_fn)
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
            finally = deregister_and_dec()
          )
        }
      }

      deregister_callback <- private$event_callbacks[[event]]$add(callback)

      domain <- find_domain(event)
      private$inc_event_callback_count(domain)

      # We'll wrap deregister_callback in another function which also keeps
      # count to the number of callbacks for the domain.
      deregister_called <- FALSE
      deregister_and_dec <- function() {
        # Make sure that if this is called multiple times that it doesn't keep
        # having effects.
        if (deregister_called)
          return()
        deregister_called <<- TRUE

        deregister_callback()
        private$dec_event_callback_count(domain)
      }

      deregister_and_dec
    },

    inc_event_callback_count = function(domain) {
      if (is.null(private$event_callback_counts[[domain]])) {
        private$event_callback_counts[[domain]] <- 0
      }

      private$event_callback_counts[[domain]] <- private$event_callback_counts[[domain]] + 1

      private$debug_log("Callbacks for ", domain, "++: ", private$event_callback_counts[[domain]])

      # If we're doing auto events and we're going from 0 to 1, enable events
      # for this domain. (Some domains do not require or have an .enable
      # method.)
      if (private$auto_events &&
          private$event_callback_counts[[domain]] == 1 &&
          isTRUE(private$event_enable_domains[[domain]]))
      {
        private$debug_log("Enabling events for ", domain)
        self[[domain]]$enable()
      }

      invisible(private$event_callback_counts[[domain]])
    },

    dec_event_callback_count = function(domain) {
      private$event_callback_counts[[domain]] <- private$event_callback_counts[[domain]] - 1

      private$debug_log("Callbacks for ", domain, "--: ", private$event_callback_counts[[domain]])
      # If we're doing auto events and we're going from 1 to 0, disable
      # enable events for this domain.
      if (private$auto_events &&
          private$event_callback_counts[[domain]] == 0 &&
          isTRUE(private$event_enable_domains[[domain]]))
      {
        private$debug_log("Disabling events for ", domain)
        self[[domain]]$disable()
      }

      invisible(private$event_callback_counts[[domain]])
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

    # =========================================================================
    # Message handling and dispatch
    # =========================================================================
    debug_messages_ = FALSE,
    debug_message_max_length = 1000,

    on_message = function(msg) {
      private$debug_log("RECV ", msg$data)
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
    },

    debug_log = function(...) {
      txt <- paste0(..., collapse = "")
      if (private$debug_messages_) {
        message(txt)
      }
    }
  )
)
