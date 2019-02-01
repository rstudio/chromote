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

      private$on_message_callbacks <- new.env(parent = emptyenv())

      private$ws$onMessage(private$on_message)
      private$ws$connect()

      # Populate methods
      protocol <- jsonlite::fromJSON(private$url("/json/protocol"), simplifyVector = FALSE)
      p <- process_protocol(protocol, self$.__enclos_env__)
      list2env(p, self)
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
    on_message_callbacks = NULL,

    send = function(msg, callback = NULL) {
      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      private$ws$send(toJSON(msg, auto_unbox = TRUE))

      if (!is.null(callback)) {
        private$add_on_message_callback(msg$id, callback)
      }
    },

    add_on_message_callback = function(id, callback) {
      id <- as.character(id)
      private$on_message_callbacks[[id]] <- callback
    },
    on_message = function(msg) {
      data <- fromJSON(msg$data)
      id <- as.character(data$id)
      if (length(id) == 0 || id == "") return()

      callback <- private$on_message_callbacks[[id]]
      if (is.function(callback)) {
        callback(data)
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
