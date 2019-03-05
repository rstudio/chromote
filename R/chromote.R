#' @importFrom websocket WebSocket
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @import promises later
#' @export
Chromote <- R6Class(
  "Chromote",
  lock_objects = FALSE,
  public = list(

    initialize = function(browser = default_browser(),
      multi_session = TRUE,
      auto_events = TRUE
    ) {
      private$browser       <- browser
      private$auto_events   <- auto_events
      private$multi_session <- multi_session

      if (multi_session) {
        chrome_info <- fromJSON(private$url("/json/version"))
      } else {
        chrome_info <- fromJSON(private$url("/json"))
      }

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
        protocol_spec <- jsonlite::fromJSON(private$url("/json/protocol"), simplifyVector = FALSE)
        self$protocol <- process_protocol(protocol_spec, self$.__enclos_env__)
        # self$protocol is a list of domains, each of which is a list of
        # methods. Graft the entries from self$protocol onto self
        list2env(self$protocol, self)

        private$event_manager <- EventManager$new(self)

        private$schedule_child_loop()
        self$wait_for(p)

        if (multi_session) {
          # At this point we're in synchronous mode, so we can use straight-line code.
          # Get the sessionId for the first (already existing) session.
          targets      <- self$protocol$Target$getTargets()
          tid          <- targets$targetInfos[[1]]$targetId
          session_info <- self$protocol$Target$attachToTarget(tid, flatten = TRUE)
          session_id   <- session_info$sessionId

          session <- ChromoteSession$new(self, session_id)
          private$sessions[[session_id]] <- session
          self$default_session_id(session_id)
        }
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

    get_child_loop = function() {
      private$child_loop
    },

    get_auto_events = function() {
      private$auto_events
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

    create_session = function(default = FALSE) {
      hybrid_chain(
        self$protocol$Target$createTarget("about:blank"),
        function(target) {
          tid <- target$targetId
          self$protocol$Target$attachToTarget(tid, flatten = TRUE)
        },
        function(session_info) {
          session_id   <- session_info$sessionId
          session <- ChromoteSession$new(self, session_id)
          private$sessions[[session_id]] <- session

          # TODO: This isn't safe in async mode
          if (default) {
            self$default_session_id(session_id)
          }

          session
        }
      )
    },

    get_sessions = function() {
      private$sessions

    },

    default_session_id = function(session_id = NULL) {
      if (is.null(session_id)) {
        return(private$default_session_id_)
      }

      if (is.null(private$sessions[[session_id]])) {
        stop("No session found with ID ", session_id)
      }

      private$default_session_id_ <- session_id
      list2env(private$sessions[[session_id]]$protocol, self)
      invisible(self)
    },

    default_session = function(session = NULL) {
      if (is.null(session)) {
        return(private$sessions[[private$default_session_id_]])
      }

      matches <- Filter(function(s) identical(s, session), private$sessions)
      if (length(matches) == 0) {
        stop("Can't set input session as default because it is not found in the sessions list.")
      }

      private$default_session_id_ <- session$get_session_id()
      list2env(private$sessions[[private$default_session_id_]]$protocol, self)
      invisible(self)
    },

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL, sessionId = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      if (!is.null(sessionId)) {
        msg$sessionId <- sessionId
      }

      p <- promise(function(resolve, reject) {
        msg_json <- toJSON(msg, auto_unbox = TRUE)
        private$ws$send(msg_json)
        self$debug_log("SEND ", msg_json)
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
        p <- p$then(function(value) return_value <<- value)
        # Error handling?
        self$wait_for(p)
        return(return_value)

      } else {
        return(invisible(p))
      }
    },

    invoke_event_callbacks = function(event, params) {
      private$event_manager$invoke_event_callbacks(event, params)
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

    debug_log = function(...) {
      txt <- paste0(..., collapse = "")
      if (private$debug_messages_) {
        message(txt)
      }
    },

    default_timeout = 10,
    protocol = NULL
  ),

  private = list(
    browser = NULL,
    ws = NULL,

    # =========================================================================
    # Browser commands
    # =========================================================================
    last_msg_id = 0,
    command_callbacks = NULL,

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


    # # =========================================================================
    # # Browser events
    # # =========================================================================
    event_manager = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      private$event_manager$register_event_listener(event, callback, timeout)
    },

    # =========================================================================
    # Message handling and dispatch
    # =========================================================================
    debug_messages_ = FALSE,
    debug_message_max_length = 1000,

    on_message = function(msg) {
      self$debug_log("RECV ", msg$data)
      data <- fromJSON(msg$data, simplifyVector = FALSE)

      if (!is.null(data$method)) {
        # This is an event notification.
        #
        # The reason that the callback is wrapped in later() is to prevent a
        # possible race when a command response and an event notification arrive
        # in the same tick. See issue #1.
        later(function() {
          if (!is.null(data$sessionId)) {
            session <- private$sessions[[data$sessionId]]
          } else {
            session <- self
          }

          session$invoke_event_callbacks(data$method, data$params)
        })

      } else if (!is.null(data$id)) {
        # This is a response to a command.
        private$invoke_command_callback(data$id, data$result, data$error)

      } else {
        message("Don't know how to handle message: ", msg$data)
      }
    },

    # =========================================================================
    # Sessions
    # =========================================================================
    multi_session = NULL,
    sessions = list(),
    default_session_id_ = NULL,

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
