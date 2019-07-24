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
      session_id = NULL,
      width = 992,
      height = 744
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


      # Set starting size
      if (!is.null(width) || !is.null(height)) {
        info <- self$Browser$getWindowForTarget()
        info$bounds$width <- width
        info$bounds$height <- height
        self$Browser$setWindowBounds(windowId = info$windowId, bounds = info$bounds)
      }

      # Find pixelRatio for screenshots
      private$pixel_ratio <- self$Runtime$evaluate("window.devicePixelRatio")$result$value
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

    screenshot = function(
      filename = "screenshot.png",
      selector = "html",
      cliprect = NULL,
      region = c("content", "padding", "border", "margin"),
      expand = NULL,
      scale = 1,
      show = FALSE,
      sync_ = TRUE
    ) {
      region = match.arg(region)
      if (length(filename) == 0 && !show) {
        stop("Cannot have empty filename and show=FALSE")
      }

      if (!is.null(cliprect) && !(is.numeric(cliprect) && length(cliprect) == 4)) {
        stop("`cliprect` must be NULL or a numeric vector with 4 elements (for left, top, width, and height).")
      }

      if (is.null(expand)) {
        expand <- 0
      }
      if (!is.numeric(expand) ||
          !(length(expand) == 1 || length(expand) == 4)) {
        stop("`expand` must be NULL, or a numeric vector with 1 or 4 elements (for top, right, bottom, left)")
      }
      if (length(expand) == 1) {
        expand <- rep(expand, 4)
      }


      # These vars are used to store information gathered from one step to use
      # in a later step.
      visual_viewport <- NULL  # Initial viewport dimensions
      image_data      <- NULL
      overall_width   <- NULL
      overall_height  <- NULL
      root_node_id    <- NULL

      # Setup stuff for both selector and cliprect code paths.
      p <- self$Emulation$setScrollbarsHidden(hidden = TRUE, sync_ = FALSE)$
        then(function(value) {
          self$Page$getLayoutMetrics(sync_ = FALSE)
        })$
        then(function(value) {
          visual_viewport <<- value$visualViewport

          self$DOM$getDocument(sync_ = FALSE)
        })$
        then(function(value) {
          root_node_id <<- value$root$nodeId
          self$DOM$querySelector(value$root$nodeId, "html", sync_ = FALSE)
        })$
        then(function(value) {
          self$DOM$getBoxModel(value$nodeId, sync_ = FALSE)
        })$
        then(function(value) {
          overall_width  <<- value$model$width
          overall_height <<- value$model$height

          # Make viewport the same size as content -- seems to be necessary
          # on Chrome 75 for Mac, though it wasn't necessary for 72. Without
          # this, the screenshot will be the full height, but everything
          # outside the viewport area will be blank white.
          self$Emulation$setVisibleSize(
            width = overall_width,
            height = overall_height,
            sync_ = FALSE
          )

          promise(function(resolve, reject) {
            later(function() resolve(TRUE), 0.5)
          })
        })


      if (is.null(cliprect)) {
        # This code path uses the selector instead of cliprect.
        p <- p$
          then(function(value) {
            self$DOM$querySelector(root_node_id, selector, sync_ = FALSE)
          })$
          then(function(value) {
            if (value$nodeId == 0) {
              stop("Selector failed: ", selector)
            }
            self$DOM$getBoxModel(value$nodeId, sync_ = FALSE)
          })$
          then(function(value) {
            # Note: `expand` values are top, right, bottom, left.
            xmin <- value$model[[region]][[1]] - expand[4]
            xmax <- value$model[[region]][[3]] + expand[2]
            ymin <- value$model[[region]][[2]] - expand[1]
            ymax <- value$model[[region]][[6]] + expand[3]

            # We need to make sure that we don't go beyond the bounds of the
            # page.
            xmin <- max(xmin, 0)
            xmax <- min(xmax, overall_width)
            ymin <- max(ymin, 0)
            ymax <- min(ymax, overall_height)

            self$Page$captureScreenshot(
              clip = list(
                x = xmin,
                y = ymin,
                width  = xmax - xmin,
                height = ymax - ymin,
                scale = scale / private$pixel_ratio
              ),
              fromSurface = TRUE,
              sync_ = FALSE
            )
          })$
          then(function(value) {
            image_data <<- value
          })

      } else {
        # If cliprect was provided, use it instead of selector
        p <- p$
          then(function(value) {
            self$Page$captureScreenshot(
              clip = list(
                x = cliprect[[1]],
                y = cliprect[[2]],
                width  = cliprect[[3]],
                height = cliprect[[4]],
                scale = scale / private$pixel_ratio
              ),
              fromSurface = TRUE,
              sync_ = FALSE
            )
          })$
          then(function(value) {
            image_data <<- value
          })
      }

      p <- p$
        then(function(value) {
          # Restore original viewport size
          self$Emulation$setVisibleSize(
            width = visual_viewport$clientWidth,
            height = visual_viewport$clientHeight,
            sync_ = FALSE
          )

          # Un-hide scrollbars
          self$Emulation$setScrollbarsHidden(hidden = FALSE, sync_ = FALSE)
        })$
        then(function(value) {
          temp_output <- FALSE
          if (is.null(filename)) {
            temp_output <- TRUE
            filename <- tempfile("chromote-screenshot-", fileext = ".png")
            on.exit(unlink(filename))
          }

          writeBin(jsonlite::base64_dec(image_data$data), filename)
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

    screenshot_pdf = function(
      filename = "screenshot.pdf",
      pagesize = "letter",
      units = c("in", "cm"),
      landscape = FALSE,
      display_header_footer = FALSE,
      margins = c(0.5, 0.5, 0.5, 0.5),
      print_background = FALSE,
      scale = 1,
      sync_ = TRUE
    ) {
      page_sizes <- list(
        letter  = c(8.5,   11),
        legal   = c(8.5,   14),
        tabloid = c(11,    17),
        ledger  = c(17,    11),
        a0      = c(33.1,  46.8),
        a1      = c(23.4,  33.1),
        a2      = c(16.54, 23.4),
        a3      = c(11.7,  16.54),
        a4      = c(8.27,  11.7),
        a5      = c(5.83,  8.27),
        a6      = c(4.13,  5.83)
      )

      units <- match.arg(units)

      if (units == "cm") {
        margins <- margins / 2.54
      }

      if (is.character(pagesize)) {
        pagesize <- tolower(pagesize)
        pagesize <- match.arg(pagesize, names(page_sizes))
        pagesize <- page_sizes[[pagesize]]

      } else if (is.numeric(pagesize) && length(pagesize) == 2) {
        # User has passed in width and height values
        if (units == "cm") {
          pagesize <- pagesize / 2.54
        }

      } else {
        stop('`pagesize` must be one of "', paste(names(page_sizes), collapse = '", "'),
          '", or a two-element vector of width and height.')
      }

      if (length(margins) == 1) {
        margins <- rep(margins, 4)
      }

      p <- self$Page$printToPDF(
          landscape           = landscape,
          displayHeaderFooter = display_header_footer,
          printBackground     = print_background,
          scale               = scale,
          paperWidth          = pagesize[[1]],
          paperHeight         = pagesize[[2]],
          marginTop           = margins[[1]],
          marginBottom        = margins[[3]],
          marginLeft          = margins[[4]],
          marginRight         = margins[[2]],
          sync_ = FALSE
        )$
        then(function(value) {
          writeBin(jsonlite::base64_dec(value$data), filename)
          filename
        })

      if (sync_) {
        invisible(self$wait_for(p))
      } else {
        p
      }
    },

    is_active = function() {
      private$is_active_
    },

    new_session = function(sync_ = TRUE, width = 992, height = 774) {
      private$parent$new_session(sync_, width = width, height = height)
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
    pixel_ratio = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
      private$event_manager$register_event_listener(event, callback, timeout)
    }
  )
)
