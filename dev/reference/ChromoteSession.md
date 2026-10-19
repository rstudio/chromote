# ChromoteSession class

This represents one *session* in a Chromote object. Note that in the
Chrome DevTools Protocol a session is a debugging session connected to a
*target*, which is a browser window/tab or an iframe.

A single target can potentially have more than one session connected to
it, but this is not currently supported by chromote.

## Public fields

- `parent`:

  [`Chromote`](https://rstudio.github.io/chromote/dev/reference/Chromote.md)
  object

- `default_timeout`:

  Default timeout in seconds for chromote to wait for a Chrome DevTools
  Protocol response.

- `protocol`:

  Dynamic protocol implementation. For expert use only!

## Methods

### Public methods

- [`ChromoteSession$new()`](#method-ChromoteSession-initialize)

- [`ChromoteSession$view()`](#method-ChromoteSession-view)

- [`ChromoteSession$close()`](#method-ChromoteSession-close)

- [`ChromoteSession$get_viewport_size()`](#method-ChromoteSession-get_viewport_size)

- [`ChromoteSession$set_viewport_size()`](#method-ChromoteSession-set_viewport_size)

- [`ChromoteSession$go_to()`](#method-ChromoteSession-go_to)

- [`ChromoteSession$screenshot()`](#method-ChromoteSession-screenshot)

- [`ChromoteSession$screenshot_pdf()`](#method-ChromoteSession-screenshot_pdf)

- [`ChromoteSession$new_session()`](#method-ChromoteSession-new_session)

- [`ChromoteSession$get_session_id()`](#method-ChromoteSession-get_session_id)

- [`ChromoteSession$respawn()`](#method-ChromoteSession-respawn)

- [`ChromoteSession$get_target_id()`](#method-ChromoteSession-get_target_id)

- [`ChromoteSession$wait_for()`](#method-ChromoteSession-wait_for)

- [`ChromoteSession$debug_log()`](#method-ChromoteSession-debug_log)

- [`ChromoteSession$get_child_loop()`](#method-ChromoteSession-get_child_loop)

- [`ChromoteSession$send_command()`](#method-ChromoteSession-send_command)

- [`ChromoteSession$get_auto_events()`](#method-ChromoteSession-get_auto_events)

- [`ChromoteSession$auto_events_enable_args()`](#method-ChromoteSession-auto_events_enable_args)

- [`ChromoteSession$invoke_event_callbacks()`](#method-ChromoteSession-invoke_event_callbacks)

- [`ChromoteSession$mark_closed()`](#method-ChromoteSession-mark_closed)

- [`ChromoteSession$is_active()`](#method-ChromoteSession-is_active)

- [`ChromoteSession$check_active()`](#method-ChromoteSession-check_active)

- [`ChromoteSession$get_init_promise()`](#method-ChromoteSession-get_init_promise)

- [`ChromoteSession$print()`](#method-ChromoteSession-print)

------------------------------------------------------------------------

### `ChromoteSession$new()`

Create a new `ChromoteSession` object.

#### Examples

    # Create a new `ChromoteSession` object.
    b <- ChromoteSession$new()

    # Create a ChromoteSession with a specific height,width
    b <- ChromoteSession$new(height = 1080, width = 1920)

    # Navigate to page
    b$go_to("http://www.r-project.org/")

    # View current chromote session
    if (interactive()) b$view()

#### Usage

    ChromoteSession$new(
      parent = default_chromote_object(),
      width = 992,
      height = 1323,
      targetId = NULL,
      wait_ = TRUE,
      auto_events = NULL,
      mobile = FALSE
    )

#### Arguments

- `parent`:

  [`Chromote`](https://rstudio.github.io/chromote/dev/reference/Chromote.md)
  object to use; defaults to
  [`default_chromote_object()`](https://rstudio.github.io/chromote/dev/reference/default_chromote_object.md)

- `width, height`:

  Width and height of the new window in integer pixel values.

- `targetId`:

  [Target](https://chromedevtools.github.io/devtools-protocol/tot/Target/)
  ID of an existing target to attach to. When a `targetId` is provided,
  the `width` and `height` arguments are ignored. If NULL (the default)
  a new target is created and attached to, and the `width` and `height`
  arguments determine its viewport size.

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  of a new `ChromoteSession` object. Otherwise, block during
  initialization, and return a `ChromoteSession` object directly.

- `auto_events`:

  If `NULL` (the default), use the `auto_events` setting from the parent
  `Chromote` object. If `TRUE`, enable automatic event
  enabling/disabling; if `FALSE`, disable automatic event
  enabling/disabling.

- `mobile`:

  Whether to emulate mobile device. When `TRUE`, Chrome updates settings
  to emulate browsing on a mobile phone; this includes viewport meta
  tag, overlay scrollbars, text autosizing and more. The default is
  `FALSE`.

#### Returns

A new `ChromoteSession` object.

------------------------------------------------------------------------

### `ChromoteSession$view()`

Display the current session in the
[`Chromote`](https://rstudio.github.io/chromote/dev/reference/Chromote.md)
browser.

If a
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
browser is being used, this method will open a new tab using your
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
browser. When not using a
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
browser, set `options(browser=)` to change the default behavior of
[`browseURL()`](https://rdrr.io/r/utils/browseURL.html).

#### Examples

    # Create a new `ChromoteSession` object.
    b <- ChromoteSession$new()

    # Navigate to page
    b$go_to("http://www.r-project.org/")

    # View current chromote session
    if (interactive()) b$view()

#### Usage

    ChromoteSession$view()

------------------------------------------------------------------------

### `ChromoteSession$close()`

Close the Chromote session.

#### Examples

    # Create a new `ChromoteSession` object.
    b <- ChromoteSession$new()

    # Navigate to page
    b$go_to("http://www.r-project.org/")

    # Close current chromote session
    b$close()

#### Usage

    ChromoteSession$close(wait_ = TRUE)

#### Arguments

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  that will resolve when the `ChromoteSession` is closed. Otherwise,
  block until the `ChromoteSession` has closed.

------------------------------------------------------------------------

### `ChromoteSession$get_viewport_size()`

Get the viewport size

#### Usage

    ChromoteSession$get_viewport_size(wait_ = TRUE)

#### Arguments

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  of a new `ChromoteSession` object. Otherwise, block during
  initialization, and return a `ChromoteSession` object directly.

#### Returns

Returns a list with values `width`, `height`, `zoom` and `mobile`. See
`$set_viewport_size()` for more details.

------------------------------------------------------------------------

### `ChromoteSession$set_viewport_size()`

Set the viewport size

Each ChromoteSession is associated with a page that may be one page open
in a browser window among many. Each page can have its own viewport
size, that can be thought of like the window size for that page.

This function uses the
[Emulation.setDeviceMetricsOverride](https://chromedevtools.github.io/devtools-protocol/tot/Emulation/#method-setDeviceMetricsOverride)
command to set the viewport size. If you need more granular control or
access to additional settings, use
`$Emulation$setDeviceMetricsOverride()`.

#### Usage

    ChromoteSession$set_viewport_size(
      width,
      height,
      zoom = NULL,
      mobile = NULL,
      wait_ = TRUE
    )

#### Arguments

- `width, height`:

  Width and height of the new window in integer pixel values.

- `zoom`:

  The zoom level of displayed content on a device, where a value of 1
  indicates normal size, greater than 1 indicates zoomed in, and less
  than 1 indicates zoomed out.

- `mobile`:

  Whether to emulate mobile device. When `TRUE`, Chrome updates settings
  to emulate browsing on a mobile phone; this includes viewport meta
  tag, overlay scrollbars, text autosizing and more. The default is
  `FALSE`.

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  of a new `ChromoteSession` object. Otherwise, block during
  initialization, and return a `ChromoteSession` object directly.

#### Returns

Invisibly returns the previous viewport dimensions so that you can
restore the viewport size, if desired.

------------------------------------------------------------------------

### `ChromoteSession$go_to()`

Navigate to a URL and wait for the page to load

This method navigates to a specified URL and waits for the page load
event to complete. This is a more reliable alternative to directly
calling `Page$navigate()`, which can return before the page is actually
loaded. This method also allows for an optional delay after the load
event has fired, in case the page needs to load additional assets after
that event.

#### Usage

    ChromoteSession$go_to(
      url,
      ...,
      delay = 0,
      callback_ = NULL,
      error_ = NULL,
      timeout_ = self$default_timeout,
      wait_ = TRUE
    )

#### Arguments

- `url`:

  The URL to navigate to.

- `...`:

  Additional parameters passed to `Page$navigate()`.

- `delay`:

  Number of seconds to wait after the page load event fires.

- `callback_`:

  Function to call when the page load event fires.

- `error_`:

  Function to call if an error occurs during navigation.

- `timeout_`:

  Maximum time in seconds to wait for the page load event (defaults to
  session's \`default_timeout“).

- `wait_`:

  If `FALSE`, returns a promise that resolves when navigation is
  complete. If `TRUE` (default), blocks until navigation is complete.

#### Returns

If `wait_` is TRUE, returns invisible(NULL). If wait\_ is FALSE, returns
a promise that resolves when navigation is complete. The promise
resolves with the value from the navigate command.

#### Examples

    # Basic navigation
    b$go_to("https://www.r-project.org")

    # Navigation with delay
    b$go_to("https://www.r-project.org", delay = 2)

    # Asynchronous navigation
    p <- b$go_to("https://www.r-project.org", wait_ = FALSE)
    p$then(function(value) print("Navigation complete!"))

------------------------------------------------------------------------

### `ChromoteSession$screenshot()`

Take a PNG screenshot

#### Examples

    # Create a new `ChromoteSession` object.
    b <- ChromoteSession$new()

    # Navigate to page
    b$go_to("http://www.r-project.org/")

    # Take screenshot
    tmppngfile <- tempfile(fileext = ".png")
    is_interactive <- interactive() # Display screenshot if interactive
    b$screenshot(tmppngfile, show = is_interactive)

    # Show screenshot file info
    unlist(file.info(tmppngfile))


    # Take screenshot using a selector
    sidebar_file <- tempfile(fileext = ".png")
    b$screenshot(sidebar_file, selector = ".sidebar", show = is_interactive)

    # ----------------------------
    # Take screenshots in parallel

    urls <- c(
      "https://www.r-project.org/",
      "https://github.com/",
      "https://news.ycombinator.com/"
    )
    # Helper method that:
    # 1. Navigates to the given URL
    # 2. Waits for the page loaded event to fire
    # 3. Takes a screenshot
    # 4. Prints a message
    # 5. Close the ChromoteSession
    screenshot_p <- function(url, filename = NULL) {
      if (is.null(filename)) {
        filename <- gsub("^.*://", "", url)
        filename <- gsub("/", "_", filename)
        filename <- gsub("\\.", "_", filename)
        filename <- sub("_$", "", filename)
        filename <- paste0(filename, ".png")
      }

      b2 <- b$new_session()
      b2$go_to(url, wait_ = FALSE)$
        then(function(value) {
          b2$screenshot(filename, wait_ = FALSE)
        })$
        then(function(value) {
          message(filename)
        })$
        finally(function() {
          b2$close()
        })
    }

    # Take multiple screenshots simultaneously
    ps <- lapply(urls, screenshot_p)
    pa <- promises::promise_all(.list = ps)$then(function(value) {
      message("Done!")
    })

    # Block the console until the screenshots finish (optional)
    b$wait_for(pa)
    #> www_r-project_org.png
    #> github_com.png
    #> news_ycombinator_com.png
    #> Done!

#### Usage

    ChromoteSession$screenshot(
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
    )

#### Arguments

- `filename`:

  File path of where to save the screenshot. The format of the
  screenshot is inferred from the file extension; use
  `options = list(format = "jpeg")` to manually choose the format. See
  [`Page.captureScreenshot`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-captureScreenshot)
  for supported formats; at the time of this release the format options
  were `"png"` (default), `"jpeg"`, or `"webp"`.

- `selector`:

  CSS selector to use for the screenshot.

- `cliprect`:

  An unnamed vector or list containing values for `top`, `left`,
  `width`, and `height`, in that order. See
  [`Page.Viewport`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#type-Viewport)
  for more information. If provided, `selector` and `expand` will be
  ignored. To provide a scale, use the `scale` parameter.

- `region`:

  CSS region to use for the screenshot.

- `expand`:

  Extra pixels to expand the screenshot. May be a single value or a
  numeric vector of top, right, bottom, left values.

- `scale`:

  Page scale factor

- `show`:

  If `TRUE`, the screenshot will be displayed in the viewer.

- `delay`:

  The number of seconds to wait before taking the screenshot after
  resizing the page. For complicated pages, this may need to be
  increased.

- `options`:

  Additional options passed to
  [`Page.captureScreenshot`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-captureScreenshot).

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  that will resolve when the `ChromoteSession` has saved the screenshot.
  Otherwise, block until the `ChromoteSession` has saved the screenshot.

------------------------------------------------------------------------

### `ChromoteSession$screenshot_pdf()`

Take a PDF screenshot

#### Examples

    # Create a new `ChromoteSession` object.
    b <- ChromoteSession$new()

    # Navigate to page
    b$go_to("http://www.r-project.org/")

    # Take screenshot
    tmppdffile <- tempfile(fileext = ".pdf")
    b$screenshot_pdf(tmppdffile)

    # Show PDF file info
    unlist(file.info(tmppdffile))

#### Usage

    ChromoteSession$screenshot_pdf(
      filename = "screenshot.pdf",
      pagesize = "letter",
      margins = 0.5,
      units = c("in", "cm"),
      landscape = FALSE,
      display_header_footer = FALSE,
      print_background = FALSE,
      scale = 1,
      wait_ = TRUE
    )

#### Arguments

- `filename`:

  File path of where to save the screenshot.

- `pagesize`:

  A single character value in the set `"letter"`, `"legal"`,
  `"tabloid"`, `"ledger"` and `"a0"` through `"a1"`. Or a numeric vector
  `c(width, height)` specifying the page size.

- `margins`:

  A numeric vector `c(top, right, bottom, left)` specifying the page
  margins.

- `units`:

  Page and margin size units. Either `"in"` or `"cm"` for inches and
  centimeters respectively.

- `landscape`:

  Paper orientation.

- `display_header_footer`:

  Display header and footer.

- `print_background`:

  Print background graphics.

- `scale`:

  Page scale factor.

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  that will resolve when the `ChromoteSession` has saved the screenshot.
  Otherwise, block until the `ChromoteSession` has saved the screnshot.

------------------------------------------------------------------------

### `ChromoteSession$new_session()`

Create a new tab / window

#### Examples

    b1 <- ChromoteSession$new()
    b1$go_to("http://www.google.com")
    b2 <- b1$new_session()
    b2$go_to("http://www.r-project.org/")
    b1$Runtime$evaluate("window.location", returnByValue = TRUE)$result$value$href
    #> [1] "https://www.google.com/"
    b2$Runtime$evaluate("window.location", returnByValue = TRUE)$result$value$href
    #> [1] "https://www.r-project.org/"

#### Usage

    ChromoteSession$new_session(
      width = 992,
      height = 1323,
      targetId = NULL,
      wait_ = TRUE
    )

#### Arguments

- `width, height`:

  Width and height of the new window.

- `targetId`:

  [Target](https://chromedevtools.github.io/devtools-protocol/tot/Target/)
  ID of an existing target to attach to. When a `targetId` is provided,
  the `width` and `height` arguments are ignored. If NULL (the default)
  a new target is created and attached to, and the `width` and `height`
  arguments determine its viewport size.

- `wait_`:

  If `FALSE`, return a
  [`promises::promise()`](https://rstudio.github.io/promises/reference/promise.html)
  that will resolve when the `ChromoteSession` has created a new
  session. Otherwise, block until the `ChromoteSession` has created a
  new session.

------------------------------------------------------------------------

### `ChromoteSession$get_session_id()`

Retrieve the session id

#### Usage

    ChromoteSession$get_session_id()

------------------------------------------------------------------------

### `ChromoteSession$respawn()`

Create a new session that connects to the same target (i.e. page) as
this session. This is useful if the session has been closed but the
target still exists.

#### Usage

    ChromoteSession$respawn()

------------------------------------------------------------------------

### `ChromoteSession$get_target_id()`

Retrieve the target id

#### Usage

    ChromoteSession$get_target_id()

------------------------------------------------------------------------

### `ChromoteSession$wait_for()`

Wait for a Chromote Session to finish. This method will block the R
session until the provided promise resolves. The loop from
`$get_child_loop()` will only advance just far enough for the promise to
resolve.

#### Examples

    b <- ChromoteSession$new()

    # Async with promise
    p <- b$Browser$getVersion(wait_ = FALSE)
    p$then(str)

    # Async with callback
    b$Browser$getVersion(wait_ = FALSE, callback_ = str)

#### Usage

    ChromoteSession$wait_for(p)

#### Arguments

- `p`:

  A promise to resolve.

------------------------------------------------------------------------

### `ChromoteSession$debug_log()`

Send a debug log message to the parent
[Chromote](https://rstudio.github.io/chromote/dev/reference/Chromote.md)
object

#### Examples

    b <- ChromoteSession$new()
    b$parent$debug_messages(TRUE)
    b$go_to("https://www.r-project.org/")
    #> SEND {"method":"Page.navigate","params":{"url":"https://www.r-project.org/"}| __truncated__}
    # Turn off debug messages
    b$parent$debug_messages(FALSE)

#### Usage

    ChromoteSession$debug_log(...)

#### Arguments

- `...`:

  Arguments pasted together with `paste0(..., collapse = "")`.

------------------------------------------------------------------------

### `ChromoteSession$get_child_loop()`

later loop.

For expert async usage only.

#### Usage

    ChromoteSession$get_child_loop()

------------------------------------------------------------------------

### `ChromoteSession$send_command()`

Send command through Chrome DevTools Protocol.

For expert use only.

#### Usage

    ChromoteSession$send_command(
      msg,
      callback = NULL,
      error = NULL,
      timeout = NULL
    )

#### Arguments

- `msg`:

  A JSON-serializable list containing `method`, and `params`.

- `callback`:

  Method to run when the command finishes successfully.

- `error`:

  Method to run if an error occurs.

- `timeout`:

  Number of milliseconds for Chrome DevTools Protocol execute a method.

------------------------------------------------------------------------

### `ChromoteSession$get_auto_events()`

Resolved `auto_events` value.

For internal use only.

#### Usage

    ChromoteSession$get_auto_events()

------------------------------------------------------------------------

### `ChromoteSession$auto_events_enable_args()`

Set or retrieve the `enable` command arguments for a domain. These
arguments are used for the `enable` command that is called for a domain,
e.g. `Fetch$enable()`, when accessing an event method.

#### Usage

    ChromoteSession$auto_events_enable_args(domain, ...)

#### Arguments

- `domain`:

  A command domain, e.g. `"Fetch"`.

- `...`:

  Arguments to use for auto-events for the domain. If not provided,
  returns the argument values currently in place for the domain. Use
  `NULL` to clear the enable arguments for a domain.

#### Examples

    if (interactive()) {
      b <- ChromoteSession$new(
        auto_events_enable_args = list(
          Fetch = list(handleAuthRequests = TRUE)
        )
      )

      # Get current `Fetch.enable` args
      b$auto_events_enable_args("Fetch")

      # Update the `Fetch.enable` args
      b$auto_events_enable_args("Fetch", handleAuthRequests = FALSE)

      # Reset `Fetch.enable` args
      b$auto_events_enable_args("Fetch", NULL)
    }

------------------------------------------------------------------------

### `ChromoteSession$invoke_event_callbacks()`

Immediately call all event callback methods.

For internal use only.

#### Usage

    ChromoteSession$invoke_event_callbacks(event, params)

#### Arguments

- `event`:

  A single event string

- `params`:

  A list of parameters to pass to the event callback methods.

------------------------------------------------------------------------

### `ChromoteSession$mark_closed()`

Mark a session, and optionally, the underlying target, as closed. For
internal use only.

#### Usage

    ChromoteSession$mark_closed(target_closed)

#### Arguments

- `target_closed`:

  Has the underlying target been closed as well as the active debugging
  session?

------------------------------------------------------------------------

### `ChromoteSession$is_active()`

Retrieve active status Once initialized, the value returned is `TRUE`.
If `$close()` has been called, this value will be `FALSE`.

#### Usage

    ChromoteSession$is_active()

------------------------------------------------------------------------

### `ChromoteSession$check_active()`

Check that a session is active, erroring if not.

#### Usage

    ChromoteSession$check_active()

------------------------------------------------------------------------

### `ChromoteSession$get_init_promise()`

Initial promise

For internal use only.

#### Usage

    ChromoteSession$get_init_promise()

------------------------------------------------------------------------

### `ChromoteSession$print()`

Summarise the current state of the object.

#### Usage

    ChromoteSession$print(..., verbose = FALSE)

#### Arguments

- `...`:

  Passed on to [`format()`](https://rdrr.io/r/base/format.html) when
  `verbose` = TRUE

- `verbose`:

  The print method defaults to a brief summary of the most important
  debugging info; use `verbose = TRUE` tp see the complex R6 object.

## Examples

``` r

## ------------------------------------------------
## Method `ChromoteSession$go_to()`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Basic navigation
b$go_to("https://www.r-project.org")

# Navigation with delay
b$go_to("https://www.r-project.org", delay = 2)

# Asynchronous navigation
p <- b$go_to("https://www.r-project.org", wait_ = FALSE)
p$then(function(value) print("Navigation complete!"))
} # }

## ------------------------------------------------
## Method `ChromoteSession$auto_events_enable_args()`
## ------------------------------------------------

if (interactive()) {
  b <- ChromoteSession$new(
    auto_events_enable_args = list(
      Fetch = list(handleAuthRequests = TRUE)
    )
  )

  # Get current `Fetch.enable` args
  b$auto_events_enable_args("Fetch")

  # Update the `Fetch.enable` args
  b$auto_events_enable_args("Fetch", handleAuthRequests = FALSE)

  # Reset `Fetch.enable` args
  b$auto_events_enable_args("Fetch", NULL)
}
```
