#' ChromoteSession class
#'
#' @description
#' This represents one _session_ in a Chromote object. Note that in the Chrome
#' DevTools Protocol a session is a debugging session connected to a _target_,
#' which is a browser window/tab or an iframe.
#'
#' A single target can potentially have more than one session connected to it,
#' but this is not currently supported by chromote.
#'
#' @export
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
    #'
    #' ## Examples
    #'
    #' ```r
    #' # Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Create a ChromoteSession with a specific height,width
    #' b <- ChromoteSession$new(height = 1080, width = 1920)
    #'
    #' # Navigate to page
    #' b$go_to("http://www.r-project.org/")
    #'
    #' # View current chromote session
    #' if (interactive()) b$view()
    #' ```
    #'
    #' @param parent [`Chromote`] object to use; defaults to
    #'   [default_chromote_object()]
    #' @param width,height Width and height of the new window in integer pixel
    #'   values.
    #' @param wait_ If `FALSE`, return a [promises::promise()] of a new
    #'   `ChromoteSession` object. Otherwise, block during initialization, and
    #'   return a `ChromoteSession` object directly.
    #' @param mobile Whether to emulate mobile device. When `TRUE`, Chrome
    #'   updates settings to emulate browsing on a mobile phone; this includes
    #'   viewport meta tag, overlay scrollbars, text autosizing and more. The
    #'   default is `FALSE`.
    #' @param auto_events If `NULL` (the default), use the `auto_events` setting
    #'   from the parent `Chromote` object. If `TRUE`, enable automatic
    #'   event enabling/disabling; if `FALSE`, disable automatic event
    #'   enabling/disabling.
    #' @return A new `ChromoteSession` object.
    initialize = function(
      parent = default_chromote_object(),
      width = 992,
      height = 1323,
      targetId = NULL,
      wait_ = TRUE,
      auto_events = NULL,
      mobile = FALSE
    ) {
      check_number_whole(width)
      check_number_whole(height)
      check_logical(auto_events, allow_null = TRUE)
      check_logical(mobile)
      check_logical(wait_)

      self$parent <- parent
      lockBinding("parent", self) # do not allow `$parent` to be set!

      self$default_timeout <- parent$default_timeout

      # Create a session from the Chromote. Basically the same code as
      # new_session(), but this is synchronous.
      if (is.null(targetId)) {
        # In earlier versions of chromote (< 0.5.0), we set `width` and `height`
        # in `Target.createTarget`. With legacy (old) headless mode, each new
        # session was essentially a tab in a new window. With new headless mode,
        # introduced with Chrome v128, new tabs are created in existing windows.
        # For Chrome v128-v133, `width` and `height` in `Target.createTarget`
        # were ignored completely, and for v134+ they only have an effect when
        # creating a new window, i.e. for the first ChromoteSession. We now use
        # `Emulation.setDeviceMetricsOverride` below to set the viewport
        # dimensions, which works across all versions of Chrome/headless-shell
        # regardless of the parent window size.
        p <- parent$Target$createTarget("about:blank", wait_ = FALSE)$then(
          function(value) {
            private$target_id <- value$targetId
            parent$Target$attachToTarget(
              value$targetId,
              flatten = TRUE,
              wait_ = FALSE
            )
          }
        )
      } else {
        private$target_id <- targetId
        p <- parent$Target$attachToTarget(
          targetId,
          flatten = TRUE,
          wait_ = FALSE
        )
      }

      p <- p$then(function(value) {
        private$session_id <- value$sessionId
        self$parent$register_session(self)
      })

      # Whenever a command method (like x$Page$navigate()) is executed, it calls
      # x$send_command(). This object's send_command() method calls the parent's
      # send_command() method with a sessionId -- that is how the command is
      # scoped to this session.
      self$protocol <- protocol_reassign_envs(
        parent$protocol,
        env = self$.__enclos_env__
      )
      lockBinding("protocol", self)

      # Graft the entries from self$protocol onto self
      list2env(self$protocol, self)

      private$auto_events <- auto_events
      private$event_manager <- EventManager$new(self)
      private$session_is_active <- TRUE
      private$target_is_active <- TRUE

      # Find pixelRatio for screenshots
      p <- p$then(function(value) {
        private$get_pixel_ratio()
      })

      if (is.null(targetId)) {
        # `Emulation.setDeviceMetricsOverride` is equivalent to turning on
        # responsive preview in developer tools and lets us adjust the size of
        # the viewport for the active session. This avoids setting the size of
        # the parent browser window and ensures that the viewport of the current
        # tab has dimensions that exactly match the requested `width` and
        # `height`.
        p <- p$then(function(value) {
          self$Emulation$setDeviceMetricsOverride(
            width = width,
            height = height,
            deviceScaleFactor = private$pixel_ratio,
            mobile = mobile,
            wait_ = FALSE
          )
        })
      }

      # When a target crashes, raise a warning.
      if (!is.null(self$Inspector$targetCrashed)) {
        p <- p$then(function(value) {
          self$Inspector$targetCrashed(
            timeout_ = NULL,
            wait_ = FALSE,
            function(value) {
              warning(
                "Chromote has received a Inspector.targetCrashed event. This means that the ChromoteSession has probably crashed."
              )
              # Even if no targetId nor sessionId is returned by Inspector.targetCashed
              # mark the session as closed. This will close all sessions..
              self$mark_closed(TRUE)
            }
          )
        })
      }

      if (wait_) {
        self$wait_for(p)
      } else {
        # If wait_=FALSE, then we can't use the usual strategy of just
        # returning p, because the call to ChromoteSession$new() always
        # returns the new object. Instead, we'll store it as
        # private$init_promise_, and the user can retrieve it with
        # b$get_init_promise().
        private$init_promise_ <- p$then(function(value) self)
      }
    },

    #' @description Display the current session in the [`Chromote`] browser.
    #'
    #' If a [`Chrome`] browser is being used, this method will open a new tab
    #' using your [`Chrome`] browser. When not using a [`Chrome`] browser, set
    #' `options(browser=)` to change the default behavior of [`browseURL()`].
    #'
    #' ## Examples
    #'
    #' ```r
    #' # Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$go_to("http://www.r-project.org/")
    #'
    #' # View current chromote session
    #' if (interactive()) b$view()
    #' ```
    view = function() {
      # A data frame of targets, one row per target.
      info <- fromJSON(self$parent$url("/json"))
      path <- info$devtoolsFrontendUrl[info$id == private$target_id]

      if (length(path) == 0) {
        stop("Target info not found.")
      }

      if (grepl("^https://chrome-devtools-frontend\\.appspot\\.com", path)) {
        # Chrome v135+ uses a fully-qualified appspot.com URL because some
        # flavors of Chrome do not ship with the devtools inspector (iOS,
        # Android). Using this URL requires also setting
        # `--remote-allow-origins=https://chrome-devtools-frontend.appspot.com`.
        # This is cumbersome and not required for desktop Chrome, so we instead
        # use the legacy path, while trying to guard against future changes.
        inspector_path <- "/devtools/inspector.html"
        inspector_contents <- tryCatch(
          readLines(self$parent$url(inspector_path)),
          error = function(err) character(0)
        )
        if (length(inspector_contents) > 0) {
          ws_url <- info$webSocketDebuggerUrl[info$id == private$target_id]
          ws_url <- sub("ws://", "ws=", ws_url)
          path <- paste0(inspector_path, "?", ws_url)
        }
      }

      browse_url(path, self$parent)
    },

    #' @description Close the Chromote session.
    #'
    #' ## Examples
    #'
    #' ```r
    #' # Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$go_to("http://www.r-project.org/")
    #'
    #' # Close current chromote session
    #' b$close()
    #' ```
    #'
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` is closed. Otherwise, block until the
    #' `ChromoteSession` has closed.
    close = function(wait_ = TRUE) {
      if (!private$target_is_active) {
        return(invisible())
      }

      # Even if this session calls Target.closeTarget, the response from
      # the browser is sent without a sessionId. In order to wait for the
      # correct browser response, we need to invoke this from the parent's
      # browser-level methods.
      p <- self$parent$protocol$Target$closeTarget(
        private$target_id,
        wait_ = FALSE
      )

      p <- p$then(function(value) {
        if (isTRUE(value$success)) {
          self$mark_closed(TRUE)
        }
        invisible(value$success)
      })

      if (wait_) {
        self$wait_for(p)
      } else {
        p
      }
    },

    #' @description Get the viewport size
    #'
    #' @param wait_ If `FALSE`, return a [promises::promise()] of a new
    #'   `ChromoteSession` object. Otherwise, block during initialization, and
    #'   return a `ChromoteSession` object directly.
    #'
    #' @return Returns a list with values `width`, `height`, `zoom`
    #'   and `mobile`. See `$set_viewport_size()` for more details.
    get_viewport_size = function(wait_ = TRUE) {
      check_bool(wait_)

      p <- self$Page$getLayoutMetrics(wait_ = FALSE)$then(function(value) {
        list(
          width = value$cssVisualViewport$clientWidth,
          height = value$cssVisualViewport$clientHeight
        )
      })$then(function(value) {
        list(
          width = value$width,
          height = value$height,
          zoom = private$pixel_ratio %||% 0,
          mobile = private$is_mobile
        )
      })

      if (wait_) self$wait_for(p) else p
    },

    #' @description Set the viewport size
    #'
    #' Each ChromoteSession is associated with a page that may be one page open
    #' in a browser window among many. Each page can have its own viewport size,
    #' that can be thought of like the window size for that page.
    #'
    #' This function uses the
    #' [Emulation.setDeviceMetricsOverride](https://chromedevtools.github.io/devtools-protocol/tot/Emulation/#method-setDeviceMetricsOverride)
    #' command to set the viewport size. If you need more granular control or
    #' access to additional settings, use
    #' `$Emulation$setDeviceMetricsOverride()`.
    #'
    #' @param width,height Width and height of the new window in integer pixel
    #'   values.
    #' @param zoom The zoom level of displayed content on a device, where a
    #'   value of 1 indicates normal size, greater than 1 indicates zoomed in,
    #'   and less than 1 indicates zoomed out.
    #' @param mobile Whether to emulate mobile device. When `TRUE`, Chrome
    #'   updates settings to emulate browsing on a mobile phone; this includes
    #'   viewport meta tag, overlay scrollbars, text autosizing and more. The
    #'   default is `FALSE`.
    #' @param wait_ If `FALSE`, return a [promises::promise()] of a new
    #'   `ChromoteSession` object. Otherwise, block during initialization, and
    #'   return a `ChromoteSession` object directly.
    #'
    #' @return Invisibly returns the previous viewport dimensions so that you
    #'   can restore the viewport size, if desired.
    set_viewport_size = function(
      width,
      height,
      zoom = NULL,
      mobile = NULL,
      wait_ = TRUE
    ) {
      check_number_whole(width)
      check_number_whole(height)
      check_number_decimal(zoom, allow_null = TRUE)
      check_bool(mobile, allow_null = TRUE)
      check_bool(wait_)

      prev_bounds <- NULL

      p <- self$get_viewport_size(wait_ = FALSE)$then(function(value) {
        prev_bounds <<- value

        self$Emulation$setDeviceMetricsOverride(
          width = width,
          height = height,
          deviceScaleFactor = zoom %||% private$pixel_ratio %||% 0,
          mobile = mobile %||% private$is_mobile %||% FALSE,
          wait_ = FALSE
        )
      })$then(function(value) {
        prev_bounds
      })

      if (wait_) invisible(self$wait_for(p)) else p
    },

    #' @description Navigate to a URL and wait for the page to load
    #'
    #' This method navigates to a specified URL and waits for the page load
    #' event to complete. This is a more reliable alternative to directly
    #' calling `Page$navigate()`, which can return before the page is actually
    #' loaded. This method also allows for an optional delay after the load
    #' event has fired, in case the page needs to load additional assets after
    #' that event.
    #'
    #' @param url The URL to navigate to.
    #' @param ... Additional parameters passed to `Page$navigate()`.
    #' @param delay Number of seconds to wait after the page load event fires.
    #' @param callback_ Function to call when the page load event fires.
    #' @param error_ Function to call if an error occurs during navigation.
    #' @param timeout_ Maximum time in seconds to wait for the page load event
    #'   (defaults to session's `default_timeout``).
    #' @param wait_ If `FALSE`, returns a promise that resolves when navigation
    #'   is complete. If `TRUE` (default), blocks until navigation is complete.
    #'
    #' @return If `wait_` is TRUE, returns invisible(NULL). If wait_ is FALSE,
    #'   returns a promise that resolves when navigation is complete. The
    #'   promise resolves with the value from the navigate command.
    #'
    #' @examples \dontrun{
    #' # Basic navigation
    #' b$go_to("https://www.r-project.org")
    #'
    #' # Navigation with delay
    #' b$go_to("https://www.r-project.org", delay = 2)
    #'
    #' # Asynchronous navigation
    #' p <- b$go_to("https://www.r-project.org", wait_ = FALSE)
    #' p$then(function(value) print("Navigation complete!"))
    #' }
    go_to = function(
      url,
      ...,
      delay = 0,
      callback_ = NULL,
      error_ = NULL,
      timeout_ = self$default_timeout,
      wait_ = TRUE
    ) {
      p <- self$Page$loadEventFired(
        callback_ = callback_,
        timeout_ = timeout_,
        wait_ = FALSE
      )
      result <- self$Page$navigate(url, ..., error_ = error_, wait_ = FALSE)

      if (delay > 0) {
        # After loadEventFired, wait `delay` seconds.
        p <- p$then(function(value) {
          promise(function(resolve, reject) {
            later(function() resolve(result), delay)
          })
        })
      }

      if (wait_) invisible(self$wait_for(p)) else p
    },

    #' @description Take a PNG screenshot
    #'
    #' ## Examples
    #'
    #' ```r
    #' # Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$go_to("http://www.r-project.org/")
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
    #'   b2$go_to(url, wait_ = FALSE)$
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
    #' #> Done!
    #' ```
    #'
    #' @param filename File path of where to save the screenshot. The format of
    #'   the screenshot is inferred from the file extension; use
    #'   `options = list(format = "jpeg")` to manually choose the format. See
    #'   [`Page.captureScreenshot`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-captureScreenshot)
    #'   for supported formats; at the time of this release the format options
    #'   were `"png"` (default), `"jpeg"`, or `"webp"`.
    #' @param selector CSS selector to use for the screenshot.
    #' @param cliprect An unnamed vector or list containing values for `top`,
    #'   `left`, `width`, and `height`, in that order. See
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
    #' @param options Additional options passed to
    #'   [`Page.captureScreenshot`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-captureScreenshot).
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` has saved the screenshot. Otherwise, block
    #' until the `ChromoteSession` has saved the screenshot.
    screenshot = function(
      filename = "screenshot.png",
      selector = "html",
      cliprect = NULL,
      region = c("content", "padding", "border", "margin"),
      expand = NULL,
      scale = 1,
      show = FALSE,
      delay = 0.5,
      options = list(),
      wait_ = TRUE
    ) {
      chromote_session_screenshot(
        self,
        private,
        filename = filename,
        selector = selector,
        cliprect = cliprect,
        region = region,
        expand = expand,
        scale = scale,
        show = show,
        delay = delay,
        options = options,
        wait_ = wait_
      )
    },

    #' @description Take a PDF screenshot
    #'
    #' ## Examples
    #'
    #' ```r
    #' # Create a new `ChromoteSession` object.
    #' b <- ChromoteSession$new()
    #'
    #' # Navigate to page
    #' b$go_to("http://www.r-project.org/")
    #'
    #' # Take screenshot
    #' tmppdffile <- tempfile(fileext = ".pdf")
    #' b$screenshot_pdf(tmppdffile)
    #'
    #' # Show PDF file info
    #' unlist(file.info(tmppdffile))
    #' ```
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
        self,
        private,
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
    #' ## Examples
    #'
    #' ```r
    #' b1 <- ChromoteSession$new()
    #' b1$go_to("http://www.google.com")
    #' b2 <- b1$new_session()
    #' b2$go_to("http://www.r-project.org/")
    #' b1$Runtime$evaluate("window.location", returnByValue = TRUE)$result$value$href
    #' #> [1] "https://www.google.com/"
    #' b2$Runtime$evaluate("window.location", returnByValue = TRUE)$result$value$href
    #' #> [1] "https://www.r-project.org/"
    #' ```
    #'
    #' @param width,height Width and height of the new window.
    #' @param wait_ If `FALSE`, return a [promises::promise()] that will resolve
    #' when the `ChromoteSession` has created a new session. Otherwise, block
    #' until the `ChromoteSession` has created a new session.
    new_session = function(
      width = 992,
      height = 1323,
      targetId = NULL,
      wait_ = TRUE
    ) {
      create_session(
        chromote = self$parent,
        width = width,
        height = height,
        targetId = targetId,
        wait_ = wait_
      )
    },

    #' @description
    #' Retrieve the session id
    get_session_id = function() {
      private$session_id
    },

    #' @description
    #' Create a new session that connects to the same target (i.e. page)
    #' as this session. This is useful if the session has been closed but the target still
    #' exists.
    respawn = function() {
      if (!private$target_is_active) {
        stop("Can't respawn session; target has been closed.")
      }

      create_session(
        chromote = self$parent,
        targetId = private$target_id,
        auto_events = private$auto_events
      )
    },

    #' @description
    #' Retrieve the target id
    get_target_id = function() {
      private$target_id
    },

    #' @description
    #' Wait for a Chromote Session to finish. This method will block the R
    #' session until the provided promise resolves. The loop from
    #' `$get_child_loop()` will only advance just far enough for the promise to
    #' resolve.
    #'
    #' ## Examples
    #'
    #' ```r
    #' b <- ChromoteSession$new()
    #'
    #' # Async with promise
    #' p <- b$Browser$getVersion(wait_ = FALSE)
    #' p$then(str)
    #'
    #' # Async with callback
    #' b$Browser$getVersion(wait_ = FALSE, callback_ = str)
    #' ```
    #'
    #' @param p A promise to resolve.
    wait_for = function(p) {
      self$parent$wait_for(p)
    },

    #' @description
    #' Send a debug log message to the parent [Chromote] object
    #'
    #' ## Examples
    #'
    #' ```r
    #' b <- ChromoteSession$new()
    #' b$parent$debug_messages(TRUE)
    #' b$go_to("https://www.r-project.org/")
    #' #> SEND {"method":"Page.navigate","params":{"url":"https://www.r-project.org/"}| __truncated__}
    #' # Turn off debug messages
    #' b$parent$debug_messages(FALSE)
    #' ```
    #'
    #' @param ... Arguments pasted together with `paste0(..., collapse = "")`.
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
    send_command = function(
      msg,
      callback = NULL,
      error = NULL,
      timeout = NULL
    ) {
      self$check_active()
      self$parent$send_command(
        msg,
        callback,
        error,
        timeout,
        sessionId = private$session_id
      )
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
    #' Set or retrieve the `enable` command arguments for a domain. These
    #' arguments are used for the `enable` command that is called for a domain,
    #' e.g. `Fetch$enable()`, when accessing an event method.
    #'
    #' @examples
    #' if (interactive()) {
    #'   b <- ChromoteSession$new(
    #'     auto_events_enable_args = list(
    #'       Fetch = list(handleAuthRequests = TRUE)
    #'     )
    #'   )
    #'
    #'   # Get current `Fetch.enable` args
    #'   b$auto_events_enable_args("Fetch")
    #'
    #'   # Update the `Fetch.enable` args
    #'   b$auto_events_enable_args("Fetch", handleAuthRequests = FALSE)
    #'
    #'   # Reset `Fetch.enable` args
    #'   b$auto_events_enable_args("Fetch", NULL)
    #' }
    #'
    #' @param domain A command domain, e.g. `"Fetch"`.
    #' @param ... Arguments to use for auto-events for the domain. If not
    #'   provided, returns the argument values currently in place for the
    #'   domain. Use `NULL` to clear the enable arguments for a domain.
    auto_events_enable_args = function(domain, ...) {
      dots <- dots_list(..., .named = TRUE)

      if (length(dots) == 0) {
        return(get_auto_events_enable_args(private, domain, self$parent))
      }

      set_auto_events_enable_args(self, private, domain, dots)
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

    #' @description Mark a session, and optionally, the underlying target,
    #'   as closed. For internal use only.
    #' @param target_closed Has the underlying target been closed as well as the
    #'   active debugging session?
    mark_closed = function(target_closed) {
      private$session_is_active <- FALSE
      private$target_is_active <- !target_closed
    },

    #' @description Retrieve active status
    #' Once initialized, the value returned is `TRUE`. If `$close()` has been
    #' called, this value will be `FALSE`.
    is_active = function() {
      private$session_is_active &&
        private$target_is_active &&
        self$parent$is_active()
    },

    #' @description Check that a session is active, erroring if not.
    check_active = function() {
      if (self$is_active()) {
        return()
      }

      if (private$target_is_active) {
        abort(
          c(
            "Session has been closed.",
            i = "Call session$respawn() to create a new session that connects to the same target."
          )
        )
      } else {
        abort("Session and underlying target have been closed.")
      }
    },

    #' @description Initial promise
    #'
    #' For internal use only.
    get_init_promise = function() {
      private$init_promise_
    },

    #' @description Summarise the current state of the object.
    #' @param verbose The print method defaults to a brief summary
    #'   of the most important debugging info; use `verbose = TRUE` tp
    #'   see the complex R6 object.
    #' @param ... Passed on to `format()` when `verbose` = TRUE
    print = function(..., verbose = FALSE) {
      if (verbose) {
        cat(format(self, ...), sep = "\n")
      } else {
        if (self$is_active()) {
          state <- "session + target active"
        } else if (private$target_is_active) {
          state <- "target active"
        } else {
          state <- "closed"
        }

        cat_line("<ChromoteSession> (", state, ")")
        if (self$is_active()) cat_line("  Session ID: ", self$get_session_id())
        if (private$target_is_active)
          cat_line("   Target ID: ", self$get_target_id())

        browser <- self$parent$get_browser()
        if (browser$is_local()) {
          cat_line(
            "  Parent PID: ",
            self$parent$get_browser()$get_process()$get_pid()
          )
        } else {
          cat_line(
            " Remote Host: ",
            sprintf("http://%s:%s", browser$get_host(), browser$get_port())
          )
        }
      }
      invisible(self)
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
    target_id = NULL,
    session_is_active = NULL,
    target_is_active = NULL,
    event_manager = NULL,
    auto_events = NULL,
    init_promise_ = NULL,

    # Updated when `Emulation.setDeviceMetricsOverride` is called
    is_mobile = NULL,
    pixel_ratio = NULL,

    get_pixel_ratio = function() {
      if (!is.null(private$pixel_ratio)) {
        promise_resolve(private$pixel_ratio)
      } else {
        self$Runtime$evaluate("window.devicePixelRatio", wait_ = FALSE)$then(
          function(value) {
            (private$pixel_ratio <- value$result$value)
          }
        )
      }
    },

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      self$check_active()
      private$event_manager$register_event_listener(event, callback, timeout)
    }
  )
)

# Wrapper around ChromoteSession$new() that can return a promise
create_session <- function(
  chromote = default_chromote_object(),
  width = 992,
  height = 1323,
  targetId = NULL,
  wait_ = TRUE,
  auto_events = NULL
) {
  session <- ChromoteSession$new(
    parent = chromote,
    width = width,
    height = height,
    targetId,
    auto_events = auto_events,
    wait_ = wait_
  )

  if (wait_) {
    session
  } else {
    # ChromoteSession$new() must return a ChromoteSession object so we need a
    # side-channel to return a promise
    session$get_init_promise()
  }
}
