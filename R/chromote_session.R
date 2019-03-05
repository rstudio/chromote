ChromoteSession <- R6Class("ChromoteSession",
  lock_objects = FALSE,
  public = list(
    initialize = function(parent, session_id) {
      private$parent        <- parent
      private$session_id    <- session_id

      self$protocol <- protocol_reassign_envs(parent$protocol, env = self$.__enclos_env__)

      # Graft the entries from self$protocol onto self
      list2env(self$protocol, self)

      private$event_manager <- EventManager$new(self)
    },

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL) {
      private$parent$send_command(msg, callback, error, timeout, sessionId = private$session_id)
    },

    get_parent = function() {
      private$parent
    },

    get_session_id = function() {
      private$session_id
    },

    get_child_loop = function() {
      private$parent$get_child_loop()
    },

    sync_mode = function(mode = NULL) {
      if (!is.null(mode)) {
        stop("Can't set sync mode from a ChromoteSession; it must be set on the parent Chromote object")
      }

      private$parent$sync_mode()
    },

    wait_for = function(p) {
      private$parent$wait_for(p)
    },

    get_auto_events = function() {
      private$parent$get_auto_events()
    },

    debug_log = function(...) {
      private$parent$debug_log(...)
    },

    invoke_event_callbacks = function(event, params) {
      private$event_manager$invoke_event_callbacks(event, params)
    },

    protocol = NULL
  ),

  private = list(
    parent = NULL,
    session_id = NULL,
    event_manager = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      private$event_manager$register_event_listener(event, callback, timeout)
    }
  )
)
