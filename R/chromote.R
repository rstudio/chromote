# This represents one _session_ in a ChromoteMaster object. Note that in the
# Chrome Devtools Protocol a session is a debugging interface connected to a
# _target_; a target is a browser window/tab, or an iframe. A single target
# can have more than one session connected to it.
Chromote <- R6Class("Chromote",
  lock_objects = FALSE,
  public = list(
    initialize = function(
      parent = ChromoteMaster$new(),
      session_id = NULL
    ) {
      # There are two ways of initializing a Chromote object: one is by sipmly
      # calling Chromote$new() (without a session_id), in which case a
      # ChromoteMaster object is created, and it is queried for the first
      # already-existing session. In this case, session_id will be NULL.
      # Another is by having the ChromoteMaster create a new session and the
      # Chromote object. In this case, the ChromoteMaster will pass itself as
      # the parent and supply a session_id.
      private$parent <- parent

      if (is.null(session_id)) {
        # Simply grab the first (already existing) session from the ChromoteMaster
        targets      <- parent$protocol$Target$getTargets()
        tid          <- targets$targetInfos[[1]]$targetId
        session_info <- parent$protocol$Target$attachToTarget(tid, flatten = TRUE)
        private$session_id <- session_info$sessionId

        private$parent$.__enclos_env__$private$sessions[[private$session_id]] <- self

      } else {
        private$session_id <- session_id
      }

      self$protocol <- protocol_reassign_envs(parent$protocol, env = self$.__enclos_env__)

      # Graft the entries from self$protocol onto self
      list2env(self$protocol, self)

      private$event_manager <- EventManager$new(self)
      private$is_active_ <- TRUE

      # Copy the ChromoteMaster object's screenshot method but reassign the
      # env.
      self$screenshot <- private$parent$screenshot
      environment(self$screenshot) <- self$.__enclos_env__
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

    view = function() {
      tid <- self$Target$getTargetInfo()$targetInfo$targetId

      # A data frame of targets, one row per target.
      info <- fromJSON(private$parent$url("/json"))
      path <- info$devtoolsFrontendUrl[info$id == tid]
      if (length(path) == 0) {
        stop("Target info not found.")
      }

      browseURL(private$parent$url(path))
    },

    is_active = function() {
      private$is_active_
    },

    new_session = function(sync_ = TRUE) {
      private$parent$new_session(sync_)
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
