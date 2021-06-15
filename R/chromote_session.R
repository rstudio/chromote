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
    #' @description Create a new `ChromoteSession` object.
    #' @param parent [`Chromote`] object to use; defaults to
    #'   [default_chromote_object()]
    #' @param width Width, in pixels, of the `Target` to create if `targetId` is
    #'   `NULL`
    #' @param height Height, in pixels, of the `Target` to create if `targetId` is
    #'   `NULL`
    #' @param targetId
    #'   [Target](https://chromedevtools.github.io/devtools-protocol/tot/Target)
    #'   ID of an existing target to attach to. When a `targetId` is provided, the
    #'   `width` and `height` arguments are ignored. If NULL (the default) a new
    #'   target is created and attached to, and the `width` and `height`
    #'   arguments determine its viewport size.
    #' @param wait_ If `FALSE`, return a [promises::promise()] of a new
    #'   `ChromoteSession` object. Otherwise, block during initialization, and
    #'   return a `ChromoteSession` object directly.
    #' @param auto_events If `NULL` (the default), use the `auto_events` setting
    #'   from the parent `Chromote` object. If `TRUE`, enable automatic
    #'   event enabling/disabling; if `FALSE`, disable automatic event
    #'   enabling/disabling.
    #' @return A new `ChromoteSession` object.
    initialize = function(
      parent = default_chromote_object(),
      width = 992,
      height = 744,
      targetId = NULL,
      wait_ = TRUE,
      auto_events = NULL
    ) {
      self$parent <- parent
      self$default_timeout <- parent$default_timeout

      # Create a session from the Chromote. Basically the same code as
      # new_session(), but this is synchronous.
      if (is.null(targetId)) {
        p <- parent$Target$createTarget(
          "about:blank",
          width = width,
          height = height,
          wait_ = FALSE
         )$
          then(function(value) {
            parent$Target$attachToTarget(value$targetId, flatten = TRUE, wait_ = FALSE)
          })
      } else {
        p <- parent$Target$attachToTarget(targetId, flatten = TRUE, wait_ = FALSE)
      }

      p <- p$
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

      private$auto_events <- auto_events
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

      # When a target crashes, raise a warning.
      if (!is.null(self$Inspector$targetCrashed)) {
        p <- p$
          then(function(value) {
            self$Inspector$targetCrashed(timeout_ = NULL, wait_ = FALSE, function(value) {
              warning("Chromote has received a Inspector.targetCrashed event. This means that the ChromoteSession has probably crashed.")
              # Even if no targetId nor sessionId is returned by Inspector.targetCashed
              # mark the session as closed. This will close all sessions..
              self$mark_closed()
            })
          })
      }

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

    new_session = function(width = 992, height = 774, targetId = NULL, wait_ = TRUE) {
      self$parent$new_session(width = width, height = height, targetId = targetId, wait_ = wait_)
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
      if (!is.null(private$auto_events)) {
        private$auto_events
      } else {
        self$parent$get_auto_events()
      }
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
    default_timeout = NULL,
    protocol = NULL
  ),

  private = list(
    session_id = NULL,
    is_active_ = NULL,
    event_manager = NULL,
    pixel_ratio = NULL,
    auto_events = NULL,
    init_promise_ = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
      private$event_manager$register_event_listener(event, callback, timeout)
    }
  )
)
