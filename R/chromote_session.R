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
      private$is_active_ <- TRUE
    },

    close = function() {
      hybrid_chain(
        self$Target$getTargetInfo(),
        function(target) {
          tid <- target$targetInfo$targetId
          # Even if this session calls Target.closeTarget, the response from
          # the browser is sent without a sessionId. In order to wait for the
          # correct browser response, we need to invoke this from the parent's
          # browser-level methods.
          private$parent$protocol$Target$closeTarget(tid)
        },
        function(value) {
          if (isTRUE(value$success)) {
            self$mark_closed()
          }
        }
      )
    },

    is_active = function() {
      private$is_active_
    },

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
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
        stop("Can't set sync mode from a ChromoteSession; it must be set on the parent Chromote object.")
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

    mark_closed = function() {
      private$is_active_ <- FALSE
    },

    protocol = NULL
  ),

  private = list(
    parent = NULL,
    session_id = NULL,
    is_active_ = NULL,
    event_manager = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
      private$event_manager$register_event_listener(event, callback, timeout)
    }
  )
)
