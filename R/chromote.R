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
      private$ws$onMessage(private$onMessage)
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
    msg_id = 0,

    send = function(msg) {
      msg$id <- private$msg_id
      private$msg_id <- private$msg_id + 1

      private$ws$send(toJSON(msg, auto_unbox = TRUE))
    },
    onMessage = function(msg) {
      on_message(msg)
    },

    url = function(path = NULL) {
      if (!is.null(path) && substr(path, 1, 1) != "/") {
        stop('path must be NULL or a string that starts with "/"')
      }
      paste0("http://127.0.0.1:", private$port, path)
    }
  )
)
