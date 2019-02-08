#' @importFrom websocket WebSocket
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
#' @import promises later
#' @export
Chromote <- R6Class(
  "Chromote",
  lock_objects = FALSE,
  public = list(

    initialize = function() {
      res <- ensure_browser_running()
      private$process <- res$process
      private$port    <- res$port

      chrome_info <- fromJSON(private$url("/json"))

      private$ws <- WebSocket$new(
        chrome_info$webSocketDebuggerUrl,
        autoConnect = FALSE
      )

      private$message_callbacks <- new.env(parent = emptyenv())
      private$event_callbacks   <- new.env(parent = emptyenv())

      private$ws$onMessage(private$on_message)
      private$ws$connect()

      # Populate methods while the connection is being established.
      protocol <- jsonlite::fromJSON(private$url("/json/protocol"), simplifyVector = FALSE)
      p <- process_protocol(protocol, self$.__enclos_env__)
      list2env(p, self)

      ws_poll_until_connected(private$ws)
    },

    view = function() {
      browseURL(private$url())
    }
  ),
  private = list(
    process = NULL,
    port = NULL,
    ws = NULL,
    last_msg_id = 0,
    message_callbacks = NULL,
    event_callbacks = NULL,

    send_command = function(msg, callback = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      p <- promise(function(resolve, reject) {
        private$ws$send(toJSON(msg, auto_unbox = TRUE))
        private$add_message_callback(msg$id, resolve)
      })

      if (!is.null(callback)) {
        p <- then(p, callback)
      }

      invisible(p)
    },


    register_event_listener = function(method_name, callback = NULL) {
      p <- promise(function(resolve, reject) {
        private$add_event_callback(method_name, resolve)
      })

      if (!is.null(callback)) {
        p <- then(p, callback)
      }

      invisible(p)
    },

    add_message_callback = function(id, callback) {
      id <- as.character(id)
      private$message_callbacks[[id]] <- callback
    },

    add_event_callback = function(event, callback) {
      # This appends callback to a list, creating list if it doesn't exist.
      private$event_callbacks[[event]] <- c(private$event_callbacks[[event]], callback)
    },

    on_message = function(msg) {
      data <- fromJSON(msg$data, simplifyVector = FALSE)

      if (!is.null(data$method)) {
        # This path handles event notifications.
        #
        # The reason that the callback is wrapped in later() is to prevent a
        # possible race when a command response and an event notification arrive
        # in the same tick. See issue #1.
        later(function() {
          # Fetch the set of callbacks, execute them, and clear the callbacks.
          method <- data$method
          # private$event_callbacks can be list, empty list, or NULL.
          for (callback in private$event_callbacks[[method]]) {
            tryCatch(
              callback(data),
              error = function(e) {
                message(
                  "Error when executing callback for ", method, ":\n  ",
                  e$message
                )
              }
            )
          }
          private$event_callbacks[[method]] <- NULL
        })

      } else if (!is.null(data$id)) {
        # This path handles responses to commands.
        id <- as.character(data$id)
        if (length(id) == 0 || id == "") return()

        callback <- private$message_callbacks[[id]]
        # Deregister callback
        private$message_callbacks[[id]] <- NULL
        if (is.function(callback)) {
          callback(data$result)
        }
      }
    },

    url = function(path = NULL) {
      if (!is.null(path) && substr(path, 1, 1) != "/") {
        stop('path must be NULL or a string that starts with "/"')
      }
      paste0("http://127.0.0.1:", private$port, path)
    }
  )
)
