#' @importFrom websocket WebSocket
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom R6 R6Class
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

      # Wait up to 5 seconds for websocket connection to be open.
      connected <- FALSE
      end <- Sys.time() + 5
      while (!connected && Sys.time() < end) {
        # Need to run the event loop for websocket to complete connection.
        later::run_now(0)
        ready_state <- private$ws$readyState()
        if (ready_state == 0L) {
          Sys.sleep(0.1)
        } else if (ready_state == 1L) {
          connected <- TRUE
          break
        } else {
          warning("Websocket connection to browser closed for some reason.")
          break
        }
      }
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

    send = function(msg, callback = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      private$ws$send(toJSON(msg, auto_unbox = TRUE))

      if (!is.null(callback)) {
        private$add_message_callback(msg$id, callback)
      }
    },

    add_message_callback = function(id, callback) {
      id <- as.character(id)
      private$message_callbacks[[id]] <- callback
    },

    add_event_callback = function(event, callback) {
      private$event_callbacks[[event]] <- callback
    },

    on_message = function(msg) {
      data <- fromJSON(msg$data)

      if (!is.null(data$method)) {
        method <- data$method
        callback <- private$event_callbacks[[method]]
        if (!is.null(callback)) {
          callback(data)
        }

      } else if (!is.null(data$id)) {
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
