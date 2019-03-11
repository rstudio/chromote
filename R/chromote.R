# This represents one _session_ in a ChromoteMaster object. Note that in the
# Chrome Devtools Protocol a session is a debugging interface connected to a
# _target_; a target is a browser window/tab, or an iframe. A single target
# can have more than one session connected to it.

#' @export
Chromote <- R6Class("Chromote",
  lock_objects = FALSE,
  cloneable = FALSE,
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
        # Create a session from the ChromoteMaster. Basically the same code as
        # new_session(), but this is synchronous.
        target <- parent$Target$createTarget("about:blank")
        tid <- target$targetId
        session_info <- parent$Target$attachToTarget(tid, flatten = TRUE)
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
    },

    close = function(sync_ = TRUE) {
      p <- self$Target$getTargetInfo(sync_ = FALSE)
      p <- p$then(function(target) {
        tid <- target$targetInfo$targetId
        # Even if this session calls Target.closeTarget, the response from
        # the browser is sent without a sessionId. In order to wait for the
        # correct browser response, we need to invoke this from the parent's
        # browser-level methods.
        private$parent$protocol$Target$closeTarget(tid, sync_ = FALSE)
      })
      p <- p$then(function(value) {
        if (isTRUE(value$success)) {
          self$mark_closed()
        }
        invisible(value$success)
      })

      if (sync_) {
        self$wait_for(p)
      } else {
        p
      }
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

    #TODO: Make async
    screenshot = function(
      selector = "body",
      filename = "screenshot.png",
      region = c("content", "padding", "border", "margin"),
      scale = 1,
      show = interactive(),
      sync_ = TRUE
    ) {
      region = match.arg(region)
      if (length(filename) == 0 && !show) {
        stop("Cannot have empty filename and show=FALSE")
      }

      p <- self$DOM$getDocument(sync_ = FALSE)$
        then(function(value) {
          self$DOM$querySelector(value$root$nodeId, selector, sync_ = FALSE)
        })$
        then(function(value) {
          if (value$nodeId == 0) {
            stop("Selector failed")
          }
          self$DOM$getBoxModel(value$nodeId, sync_ = FALSE)
        })$
        then(function(value) {
          if (is.null(value)) {
            stop("Selector failed")
          }
          xmin <- value$model[[region]][[1]]
          xmax <- value$model[[region]][[3]]
          ymin <- value$model[[region]][[2]]
          ymax <- value$model[[region]][[6]]
          self$Page$captureScreenshot(clip = list(
            x = xmin,
            y = ymin,
            width  = xmax - xmin,
            height = ymax - ymin,
            scale = scale
          ), sync_ = FALSE)
        })$
        then(function(value) {
          temp_output <- FALSE
          if (is.null(filename)) {
            temp_output <- TRUE
            filename <- tempfile("chromote-screenshot-", fileext = ".png")
            on.exit(unlink(filename))
          }

          writeBin(jsonlite::base64_dec(value$data), filename)
          if (show) {
            showimage::show_image(filename)
          }

          if (temp_output) {
            invisible()
          } else {
            invisible(filename)
          }
        })$
        catch(function(err) {
          warning("An error occurred: ", err)
        })

      if (sync_) {
        self$wait_for(p)
      } else {
        p
      }
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
