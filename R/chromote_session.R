# This represents one _session_ in a Chromote object. Note that in the Chrome
# DevTools Protocol a session is a debugging interface connected to a
# _target_; a target is a browser window/tab, or an iframe. A single target
# can have more than one session connected to it.

#' ChromoteSession class
#' @export
#' @param timeout_ Number of seconds for \pkg{chromote} to wait for a Chrome
#' DevTools Protocol response. If `timeout_` is [`rlang::missing_arg()`] and
#' `timeout` is provided, `timeout_` will be set to `2 * timeout / 1000`.
#' @param timeout Number of milliseconds for Chrome DevTools Protocol execute a
#' method.
#' @param width Width, in pixels, of the `Target` to create if `targetId` is
#'   `NULL`
#' @param height Height, in pixels, of the `Target` to create if `targetId` is
#'   `NULL`
#' @param targetId
#'   [Target](https://chromedevtools.github.io/devtools-protocol/tot/Target/)
#'   ID of an existing target to attach to. When a `targetId` is provided, the
#'   `width` and `height` arguments are ignored. If NULL (the default) a new
#'   target is created and attached to, and the `width` and `height`
#'   arguments determine its viewport size.
ChromoteSession <- R6Class(
  "ChromoteSession",
  lock_objects = FALSE,
  cloneable = FALSE,
  public = list(
    #' @description Create a new `ChromoteSession` object.
    #' @param parent [`Chromote`] object to use; defaults to
    #'   [default_chromote_object()]
    #' @param auto_events If `NULL` (the default), use the `auto_events` setting
    #'   from the parent `Chromote` object. If `TRUE`, enable automatic
    #'   event enabling/disabling; if `FALSE`, disable automatic event
    #'   enabling/disabling.
    #' @param wait_ If `FALSE`, return a [promises::promise()] of a new
    #'   `ChromoteSession` object. Otherwise, block during initialization, and
    #'   return a `ChromoteSession` object directly.
    #' @return A new `ChromoteSession` object.
    #' @examples
    #' \dontrun{# Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Create a ChromoteSession with a specific height,width
    #' b <- ChromoteSession$new(height = 1080, width = 1920)
    #'
    #' # Navigate to page
    #' b$Page$navigate("http://www.r-project.org/")
    #'
    #' # View current chromote session
    #' if (interactive()) b$view()}
    initialize = function(
      parent = default_chromote_object(),
      width = 992,
      height = 1323,
      targetId = NULL,
      wait_ = TRUE,
      auto_events = NULL
    ) {
      self$parent <- parent
      lockBinding("parent", self) # do not allow `$parent` to be set!

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
      lockBinding("protocol", self)

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


    #' @description Display the current session in the [`Chromote`] browser.
    #'
    #' If a [`Chrome`] browser is being used, this method will open a new tab
    #' using your [`Chrome`] browser. When not using a [`Chrome`] browser, set
    #' `options(browser=)` to change the default behavior of [`browseURL()`].
    #' @examples
    #' \dontrun{# Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$Page$navigate("http://www.r-project.org/")
    #'
    #' # View current chromote session
    #' if (interactive()) b$view()}
    view = function() {
      tid <- self$Target$getTargetInfo()$targetInfo$targetId

      # A data frame of targets, one row per target.
      info <- fromJSON(self$parent$url("/json"))
      path <- info$devtoolsFrontendUrl[info$id == tid]
      if (length(path) == 0) {
        stop("Target info not found.")
      }

      browse_url(path, self$parent)
    },

    #' @description Close the Chromote session.
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` is closed. Otherwise, block until the
    #' `ChromoteSession` has closed.
    #' @examples
    #' \dontrun{# Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$Page$navigate("http://www.r-project.org/")
    #'
    #' # Close current chromote session
    #' b$close()}
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

    #' @description Take a PNG screenshot
    #'
    #' @param filename File path of where to save the screenshot.
    #' @param selector CSS selector to use for the screenshot.
    #' @param cliprect A list containing `x`, `y`, `width`, and `height`. See
    #' [`Page.Viewport`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#type-Viewport)
    #' for more information. If provided, `selector` and `expand` will be
    #' ignored. To provide a scale, use the `scale` parameter.
    #' @param region CSS region to use for the screenshot.
    #' @param expand Extra pixels to expand the screenshot. May be a single
    #' value or a numeric vector of top, right, bottom, left values.
    #' @param scale Page scale factor
    #' @param show If `TRUE`, the screenshot will be displayed in the viewer.
    #' @param delay The number of seconds to wait before taking the screenshot
    #' after resizing the page. For complicated pages, this may need to be
    #' increased.
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` has saved the screenshot. Otherwise, block
    #' until the `ChromoteSession` has saved the screenshot.
    #' @examples
    #' \dontrun{# Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$Page$navigate("http://www.r-project.org/")
    #'
    #' # Take screenshot
    #' tmppngfile <- tempfile(fileext = ".png")
    #' is_interactive <- interactive() # Display screenshot if interactive
    #' b$screenshot(tmppngfile, show = is_interactive)
    #'
    #' # Show screenshot file info
    #' unlist(file.info(tmppngfile))
    #'
    #'
    #' # Take screenshot using a selector
    #' sidebar_file <- tempfile(fileext = ".png")
    #' b$screenshot(sidebar_file, selector = ".sidebar", show = is_interactive)
    #'
    #' # ----------------------------
    #' # Take screenshots in parallel
    #'
    #' urls <- c(
    #'   "https://www.r-project.org/",
    #'   "https://github.com/",
    #'   "https://news.ycombinator.com/"
    #' )
    #' # Helper method that:
    #' # 1. Navigates to the given URL
    #' # 2. Waits for the page loaded event to fire
    #' # 3. Takes a screenshot
    #' # 4. Prints a message
    #' # 5. Close the ChromoteSession
    #' screenshot_p <- function(url, filename = NULL) {
    #'   if (is.null(filename)) {
    #'     filename <- gsub("^.*://", "", url)
    #'     filename <- gsub("/", "_", filename)
    #'     filename <- gsub("\\.", "_", filename)
    #'     filename <- sub("_$", "", filename)
    #'     filename <- paste0(filename, ".png")
    #'   }
    #'
    #'   b2 <- b$new_session()
    #'   b2$Page$navigate(url, wait_ = FALSE)
    #'   b2$Page$loadEventFired(wait_ = FALSE)$
    #'     then(function(value) {
    #'       b2$screenshot(filename, wait_ = FALSE)
    #'     })$
    #'     then(function(value) {
    #'       message(filename)
    #'     })$
    #'     finally(function() {
    #'       b2$close()
    #'     })
    #' }
    #'
    #' # Take multiple screenshots simultaneously
    #' ps <- lapply(urls, screenshot_p)
    #' pa <- promises::promise_all(.list = ps)$then(function(value) {
    #'   message("Done!")
    #' })
    #'
    #' # Block the console until the screenshots finish (optional)
    #' b$wait_for(pa)
    #' #> www_r-project_org.png
    #' #> github_com.png
    #' #> news_ycombinator_com.png
    #' #> Done!}
    screenshot = function(
      filename = "screenshot.png",
      selector = "html",
      cliprect = NULL,
      region = c("content", "padding", "border", "margin"),
      expand = NULL,
      scale = 1,
      show = FALSE,
      delay = 0.5,
      wait_ = TRUE
    ) {
      chromote_session_screenshot(
        self, private,
        filename = filename,
        selector = selector,
        cliprect = cliprect,
        region = region,
        expand = expand,
        scale = scale,
        show = show,
        delay = delay,
        wait_ = wait_
      )
    },

    #' @description Take a PDF screenshot
    #'
    #' @param filename File path of where to save the screenshot.
    #' @param pagesize A single character value in the set `"letter"`,
    #' `"legal"`, `"tabloid"`, `"ledger"` and `"a0"` through `"a1"`. Or a
    #' numeric vector `c(width, height)` specifying the page size.
    #' @param margins A numeric vector `c(top, right, bottom, left)` specifying
    #' the page margins.
    #' @param units Page and margin size units. Either `"in"` or `"cm"` for
    #' inches and centimeters respectively.
    #' @param landscape Paper orientation.
    #' @param display_header_footer Display header and footer.
    #' @param print_background Print background graphics.
    #' @param scale Page scale factor.
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` has saved the screenshot. Otherwise, block
    #' until the `ChromoteSession` has saved the screnshot.
    #' @examples
    #' \dontrun{# Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$Page$navigate("http://www.r-project.org/")
    #'
    #' # Take screenshot
    #' tmppdffile <- tempfile(fileext = ".pdf")
    #' b$screenshot_pdf(tmppdffile)
    #'
    #' # Show PDF file info
    #' unlist(file.info(tmppdffile))}
    screenshot_pdf = function(
      filename = "screenshot.pdf",
      pagesize = "letter",
      margins = 0.5,
      units = c("in", "cm"),
      landscape = FALSE,
      display_header_footer = FALSE,
      print_background = FALSE,
      scale = 1,
      wait_ = TRUE
    ) {
      chromote_session_screenshot_pdf(
        self, private,
        filename = filename,
        pagesize = pagesize,
        margins = margins,
        units = units,
        landscape = landscape,
        display_header_footer = display_header_footer,
        print_background = print_background,
        scale = scale,
        wait_ = wait_
      )
    },

    #' @description Create a new tab / window
    #'
    #' @param width,height Width and height of the new window.
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` has created a new session. Otherwise, block
    #' until the `ChromoteSession` has created a new session.
    #' @examples
    #' \dontrun{b1 <- ChromoteSession$new()
    #' b1$Page$navigate("http://www.google.com")
    #' b2 <- b1$new_session()
    #' b2$Page$navigate("http://www.r-project.org/")
    #' b1$Runtime$evaluate("window.location", returnByValue = TRUE)$result$value$href
    #' #> [1] "https://www.google.com/"
    #' b2$Runtime$evaluate("window.location", returnByValue = TRUE)$result$value$href
    #' #> [1] "https://www.r-project.org/"}
    new_session = function(width = 992, height = 1323, targetId = NULL, wait_ = TRUE) {
      self$parent$new_session(width = width, height = height, targetId = targetId, wait_ = wait_)
    },

    #' @description
    #' Retrieve the session id
    #' @examples
    #' \dontrun{b <- ChromoteSession$new()
    #' b$get_session_id()
    #' #> [1] "05764F1D439F4292497A21C6526575DA"}
    get_session_id = function() {
      private$session_id
    },

    #' @description
    #' Wait for a Chromote Session to finish. This method will block the R
    #' session until the provided promise resolves. The loop from
    #' `$get_child_loop()` will only advance just far enough for the promise to
    #' resolve.
    #' @param p A promise to resolve.
    #' @examples
    #' \dontrun{b <- ChromoteSession$new()
    #'
    #' # Async with promise
    #' p <- b$Browser$getVersion(wait_ = FALSE)
    #' p$then(str)
    #'
    #' # Async with callback
    #' b$Browser$getVersion(wait_ = FALSE, callback_ = str)}
    wait_for = function(p) {
      self$parent$wait_for(p)
    },

    #' @description
    #' Send a debug log message to the parent [Chromote] object
    #'
    #' @param ... Arguments pasted together with `paste0(..., collapse = "")`.
    #' @examples
    #' \dontrun{b <- ChromoteSession$new()
    #' b$parent$debug_messages(TRUE)
    #' b$Page$navigate("https://www.r-project.org/")
    #' #> SEND {"method":"Page.navigate","params":{"url":"https://www.r-project.org/"}| __truncated__}
    #' # Turn off debug messages
    #' b$parent$debug_messages(FALSE)}
    debug_log = function(...) {
      self$parent$debug_log(...)
    },

    #' @description
    #' \pkg{later} loop.
    #'
    #' For expert async usage only.
    get_child_loop = function() {
      self$parent$get_child_loop()
    },

    #' @description
    #' Send command through Chrome DevTools Protocol.
    #'
    #' For expert use only.
    #' @param msg A JSON-serializable list containing `method`, and `params`.
    #' @param callback Method to run when the command finishes successfully.
    #' @param error Method to run if an error occurs.
    #' @param timeout Number of milliseconds for Chrome DevTools Protocol
    #' execute a method.
    send_command = function(msg, callback = NULL, error = NULL, timeout = NULL) {
      if (!private$is_active_) {
        stop("Session ", private$session_id, " is closed.")
      }
      self$parent$send_command(msg, callback, error, timeout, sessionId = private$session_id)
    },

    #' @description
    #' Resolved `auto_events` value.
    #'
    #' For internal use only.
    get_auto_events = function() {
      if (!is.null(private$auto_events)) {
        private$auto_events
      } else {
        self$parent$get_auto_events()
      }
    },

    #' @description
    #' Immediately call all event callback methods.
    #'
    #' For internal use only.
    #' @param event A single event string
    #' @param params A list of parameters to pass to the event callback methods.
    invoke_event_callbacks = function(event, params) {
      private$event_manager$invoke_event_callbacks(event, params)
    },

    #' @description
    #' Disable callbacks for a given session.
    #'
    #' For internal use only.
    mark_closed = function() {
      private$is_active_ <- FALSE
    },

    #' @description Retrieve active status
    #' Once initialized, the value returned is `TRUE`. If `$close()` has been
    #' called, this value will be `FALSE`.
    is_active = function() {
      private$is_active_
    },

    #' @description Initial promise
    #'
    #' For internal use only.
    init_promise = function() {
      private$init_promise_
    },

    #' @field parent [`Chromote`] object
    parent = NULL,
    #' @field default_timeout Default timeout in seconds for \pkg{chromote} to
    #' wait for a Chrome DevTools Protocol response.
    default_timeout = NULL,
    #' @field protocol Dynamic protocol implementation. For expert use only!
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
