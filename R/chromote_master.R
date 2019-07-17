#' @importFrom websocket WebSocket
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @import promises later
#' @export
ChromoteMaster <- R6Class(
  "ChromoteMaster",
  lock_objects = FALSE,
  cloneable = FALSE,
  public = list(

    initialize = function(
      browser = default_browser(),
      multi_session = TRUE,
      auto_events = TRUE
    ) {
      private$browser       <- browser
      private$auto_events   <- auto_events
      private$multi_session <- multi_session

      if (multi_session) {
        chrome_info <- fromJSON(self$url("/json/version"))
      } else {
        chrome_info <- fromJSON(self$url("/json"))
      }

      private$command_callbacks <- new.env(parent = emptyenv())

      private$parent_loop <- current_loop()

      # Use a private event loop to drive the websocket
      private$child_loop <- create_loop(autorun = FALSE)

      with_loop(private$child_loop, {
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
        protocol_spec <- jsonlite::fromJSON(self$url("/json/protocol"), simplifyVector = FALSE)
        self$protocol <- process_protocol(protocol_spec, self$.__enclos_env__)
        # self$protocol is a list of domains, each of which is a list of
        # methods. Graft the entries from self$protocol onto self
        list2env(self$protocol, self)

        private$event_manager <- EventManager$new(self)
        private$is_active_ <- TRUE

        private$schedule_child_loop()
        self$wait_for(p)

        private$register_default_event_listeners()
      })
    },

    view = function() {
      browseURL(self$url())
    },

    get_child_loop = function() {
      private$child_loop
    },

    get_auto_events = function() {
      private$auto_events
    },

    # This runs the child loop until the promise is resolved.
    wait_for = function(p) {
      if (!is.promise(p)) {
        stop("wait_for requires a promise object.")
      }

      synchronize(p, loop = private$child_loop)
    },

    new_session = function(sync_ = TRUE, width = 992, height = 774) {
      p <- self$protocol$Target$createTarget("about:blank", sync_ = FALSE)
      p <- p$then(function(target) {
        tid <- target$targetId
        self$protocol$Target$attachToTarget(tid, flatten = TRUE, sync_ = FALSE)
      })
      p <- p$then(function(session_info) {
        session_id <- session_info$sessionId
        session <- Chromote$new(self, session_id, width, height)
        private$sessions[[session_id]] <- session
        session
      })

      if (sync_) {
        self$wait_for(p)
      } else {
        p
      }
    },

    get_sessions = function() {
      private$sessions
    },

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL, sessionId = NULL) {
      if (!private$is_active_) {
        stop("Chromote object is closed.")
      }

      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      if (!is.null(sessionId)) {
        msg$sessionId <- sessionId
      }

      p <- promise(function(resolve, reject) {
        msg_json <- toJSON(msg, auto_unbox = TRUE)
        private$ws$send(msg_json)
        self$debug_log("SEND ", msg_json)
        # One of these callbacks will be invoked when a message arrives with a
        # matching id.
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

      p <- p$finally(function() private$remove_command_callback(msg$id))

      p
    },

    invoke_event_callbacks = function(event, params) {
      private$event_manager$invoke_event_callbacks(event, params)
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
      txt <- truncate(paste0(..., collapse = ""), 1000)
      if (private$debug_messages_) {
        message(txt)
      }
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

    default_timeout = 10,
    protocol = NULL
  ),

  private = list(
    browser = NULL,
    ws = NULL,
    is_active_ = NULL,

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

    # Invoke the callback for a command (using id).
    invoke_command_callback = function(id, value, error) {
      id <- as.character(id)

      if (!exists(id, envir = private$command_callbacks, inherits = FALSE))
        return()

      handlers <- private$command_callbacks[[id]]

      if (!is.null(error)) {
        handlers$error(error)

      } else if (!is.null(value)) {
        handlers$callback(value)
      }
    },

    remove_command_callback = function(id) {
      rm(list = id, envir = private$command_callbacks)
    },


    # =========================================================================
    # Browser events
    # =========================================================================
    event_manager = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Chromote object is closed.")
      }
      private$event_manager$register_event_listener(event, callback, timeout)
    },

    register_default_event_listeners = function() {
      # When a target is closed, mark the corresponding R session object as
      # closed and remove it from the list of sessions.
      self$protocol$Target$detachedFromTarget(function(msg) {
        sid <- msg$sessionId
        session <- private$sessions[[sid]]
        if (is.null(session))
          return()

        private$sessions[[sid]] <- NULL
        session$mark_closed()
      })
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
        self$debug_log("Websocket state is closed.")
        private$is_active_ <- FALSE
        return()
      }

      # This tells the parent loop to schedule one run of the child
      # (private) loop.
      with_loop(private$parent_loop,
        later(private$run_child_loop)
      )

      private$child_loop_is_scheduled <- TRUE
    },

    run_child_loop = function() {
      private$child_loop_is_scheduled <- FALSE

      tryCatch(
        with_loop(private$child_loop,
          run_now()
        ),
        finally = {
          private$schedule_child_loop()
        }
      )
    }
  )
)
