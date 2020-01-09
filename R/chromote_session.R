create_blank_target <- function(parent, width, height) {
  parent$Target$createTarget(
    "about:blank",
    width = width,
    height = height,
    wait_ = FALSE
  )
}

# This represents one _session_ in a Chromote object. Note that in the Chrome
# Devtools Protocol a session is a debugging interface connected to a
# _target_; a target is a browser window/tab, or an iframe. A single target
# can have more than one session connected to it.

#' ChromoteSession class
#' @export
ChromoteSession <- R6Class(
  "ChromoteSession",
  lock_objects = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function(
      parent = default_chromote_object(),
      width = 992,
      height = 744,
      tid = create_blank_target(parent, width, height)$targetId,
      wait_ = TRUE
    ) {
      self$parent <- parent

      # Create a session from the Chromote. Basically the same code as
      # new_session(), but this is synchronous.
      p <- parent$Target$attachToTarget(tid, flatten = TRUE, wait_ = FALSE)$
        then(function(value) {
          private$session_id <- value$sessionId
          self$parent$register_session(self)
        })

      # Whenever a command method (like x$Page$navigate()) is executed, it calls
      # x$send_command(). This object's send_command() method calls the parent's
      # send_command() method with a sessionId -- that is how the command is
      # scoped to this session.
      self$protocol <- protocol_reassign_envs(parent$protocol, env = self$.__enclos_env__)

      # Graft the entries from self$protocol onto self
      list2env(self$protocol, self)

      private$event_manager <- EventManager$new(self)
      private$is_active_ <- TRUE

      # Find pixelRatio for screenshots
      p <- p$
        then(function(value) {
          self$Runtime$evaluate("window.devicePixelRatio", wait_ = FALSE)
        })$
        then(function(value) {
          private$pixel_ratio <- value$result$value
        })

      if (wait_) {
        self$wait_for(p)
      } else {
        # If wait_=FALSE, then we can't use the usual strategy of just
        # returning p, because the call to ChromoteSession$new() always
        # returns the new object. Instead, we'll store it as
        # private$init_promise_, and the user can retrieve it with
        # b$init_promise().
        private$init_promise_ <- p$then(function(value) self)
      }

    },

    init_promise = function() {
      private$init_promise_
    },

    close = function(wait_ = TRUE) {
      p <- self$Target$getTargetInfo(wait_ = FALSE)
      p <- p$then(function(target) {
        tid <- target$targetInfo$targetId
        # Even if this session calls Target.closeTarget, the response from
        # the browser is sent without a sessionId. In order to wait for the
        # correct browser response, we need to invoke this from the parent's
        # browser-level methods.
        self$parent$protocol$Target$closeTarget(tid, wait_ = FALSE)
      })
      p <- p$then(function(value) {
        if (isTRUE(value$success)) {
          self$mark_closed()
        }
        invisible(value$success)
      })

      if (wait_) {
        self$wait_for(p)
      } else {
        p
      }
    },

    view = function() {
      tid <- self$Target$getTargetInfo()$targetInfo$targetId

      # A data frame of targets, one row per target.
      info <- fromJSON(self$parent$url("/json"))
      path <- info$devtoolsFrontendUrl[info$id == tid]
      if (length(path) == 0) {
        stop("Target info not found.")
      }

      browseURL(self$parent$url(path))
    },

    is_active = function() {
      private$is_active_
    },

    new_session = function(wait_ = TRUE, width = 992, height = 774) {
      self$parent$new_session(wait_, width = width, height = height)
    },

    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
      self$parent$send_command(msg, callback, error, timeout, sessionId = private$session_id)
    },

    get_parent = function() {
      self$parent
    },

    get_session_id = function() {
      private$session_id
    },

    get_child_loop = function() {
      self$parent$get_child_loop()
    },

    wait_for = function(p) {
      self$parent$wait_for(p)
    },

    get_auto_events = function() {
      self$parent$get_auto_events()
    },

    debug_log = function(...) {
      self$parent$debug_log(...)
    },

    invoke_event_callbacks = function(event, params) {
      private$event_manager$invoke_event_callbacks(event, params)
    },

    mark_closed = function() {
      private$is_active_ <- FALSE
    },

    parent = NULL,
    protocol = NULL
  ),

  private = list(
    session_id = NULL,
    is_active_ = NULL,
    event_manager = NULL,
    pixel_ratio = NULL,
    init_promise_ = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
      private$event_manager$register_event_listener(event, callback, timeout)
    }
  )
)
