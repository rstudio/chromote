# Chromote class

A `Chromote` object represents the browser as a whole, and it can have
multiple *targets*, which each represent a browser tab. In the Chrome
DevTools Protocol, each target can have one or more debugging *sessions*
to control it. A `ChromoteSession` object represents a single *session*.

A `Chromote` object can have any number of `ChromoteSession` objects as
children. It is not necessary to create a `Chromote` object manually.
You can simply call:

    b <- ChromoteSession$new()

and it will automatically create a `Chromote` object if one has not
already been created. The chromote package will then designate that
`Chromote` object as the *default* `Chromote` object for the package, so
that any future calls to `ChromoteSession$new()` will automatically use
the same `Chromote`. This is so that it doesn't start a new browser for
every `ChromoteSession` object that is created.

## Public fields

- `default_timeout`:

  Default timeout in seconds for chromote to wait for a Chrome DevTools
  Protocol response.

- `protocol`:

  Dynamic protocol implementation. For expert use only!

## Methods

### Public methods

- [`Chromote$new()`](#method-Chromote-initialize)

- [`Chromote$connect()`](#method-Chromote-connect)

- [`Chromote$view()`](#method-Chromote-view)

- [`Chromote$get_auto_events()`](#method-Chromote-get_auto_events)

- [`Chromote$auto_events_enable_args()`](#method-Chromote-auto_events_enable_args)

- [`Chromote$get_child_loop()`](#method-Chromote-get_child_loop)

- [`Chromote$wait_for()`](#method-Chromote-wait_for)

- [`Chromote$new_session()`](#method-Chromote-new_session)

- [`Chromote$get_sessions()`](#method-Chromote-get_sessions)

- [`Chromote$register_session()`](#method-Chromote-register_session)

- [`Chromote$send_command()`](#method-Chromote-send_command)

- [`Chromote$invoke_event_callbacks()`](#method-Chromote-invoke_event_callbacks)

- [`Chromote$debug_messages()`](#method-Chromote-debug_messages)

- [`Chromote$debug_log()`](#method-Chromote-debug_log)

- [`Chromote$url()`](#method-Chromote-url)

- [`Chromote$is_active()`](#method-Chromote-is_active)

- [`Chromote$is_alive()`](#method-Chromote-is_alive)

- [`Chromote$check_active()`](#method-Chromote-check_active)

- [`Chromote$get_browser()`](#method-Chromote-get_browser)

- [`Chromote$close()`](#method-Chromote-close)

- [`Chromote$print()`](#method-Chromote-print)

------------------------------------------------------------------------

### `Chromote$new()`

#### Usage

    Chromote$new(browser = Chrome$new(), multi_session = TRUE, auto_events = TRUE)

#### Arguments

- `browser`:

  A
  [`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)
  object

- `multi_session`:

  Should multiple sessions be allowed?

- `auto_events`:

  If `TRUE`, enable automatic event enabling/disabling; if `FALSE`,
  disable automatic event enabling/disabling.

------------------------------------------------------------------------

### `Chromote$connect()`

Re-connect the websocket to the browser. The Chrome browser
automatically closes websockets when your computer goes to sleep; you
can use this to bring it back to life with a new connection.

#### Usage

    Chromote$connect(multi_session = TRUE, wait_ = TRUE)

#### Arguments

- `multi_session`:

  Should multiple sessions be allowed?

- `wait_`:

  If `FALSE`, return a promise; if `TRUE` wait until connection is
  complete.

------------------------------------------------------------------------

### `Chromote$view()`

Display the current session in the `browser`

If a
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
browser is being used, this method will open a new tab using your
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
browser. When not using a
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
browser, set `options(browser=)` to change the default behavior of
[`browseURL()`](https://rdrr.io/r/utils/browseURL.html).

#### Usage

    Chromote$view()

------------------------------------------------------------------------

### `Chromote$get_auto_events()`

`auto_events` value.

For internal use only.

#### Usage

    Chromote$get_auto_events()

------------------------------------------------------------------------

### `Chromote$auto_events_enable_args()`

Set or retrieve the `enable` command arguments for a domain. These
arguments are used for the `enable` command that is called for a domain,
e.g. `Fetch$enable()`, when accessing an event method.

#### Usage

    Chromote$auto_events_enable_args(domain, ...)

#### Arguments

- `domain`:

  A command domain, e.g. `"Fetch"`.

- `...`:

  Arguments to use for auto-events for the domain. If not provided,
  returns the argument values currently in place for the domain. Use
  `NULL` to clear the enable arguments for a domain.

------------------------------------------------------------------------

### `Chromote$get_child_loop()`

Local later loop.

For expert async usage only.

#### Usage

    Chromote$get_child_loop()

------------------------------------------------------------------------

### `Chromote$wait_for()`

Wait until the promise resolves

Blocks the R session until the promise (`p`) is resolved. The loop from
`$get_child_loop()` will only advance just far enough for the promise to
resolve.

#### Usage

    Chromote$wait_for(p)

#### Arguments

- `p`:

  A promise to resolve.

------------------------------------------------------------------------

### `Chromote$new_session()`

Create a new tab / window

#### Usage

    Chromote$new_session(width = 992, height = 1323, targetId = NULL, wait_ = TRUE)

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
  of a new `ChromoteSession` object. Otherwise, block during
  initialization, and return a `ChromoteSession` object directly.

------------------------------------------------------------------------

### `Chromote$get_sessions()`

Retrieve all
[`ChromoteSession`](https://rstudio.github.io/chromote/dev/reference/ChromoteSession.md)
objects

#### Usage

    Chromote$get_sessions()

#### Returns

A list of `ChromoteSession` objects

------------------------------------------------------------------------

### `Chromote$register_session()`

Register
[`ChromoteSession`](https://rstudio.github.io/chromote/dev/reference/ChromoteSession.md)
object

#### Usage

    Chromote$register_session(session)

#### Arguments

- `session`:

  A `ChromoteSession` object

  For internal use only.

------------------------------------------------------------------------

### `Chromote$send_command()`

Send command through Chrome DevTools Protocol.

For expert use only.

#### Usage

    Chromote$send_command(
      msg,
      callback = NULL,
      error = NULL,
      timeout = NULL,
      sessionId = NULL
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

- `sessionId`:

  Determines which
  [`ChromoteSession`](https://rstudio.github.io/chromote/dev/reference/ChromoteSession.md)
  with the corresponding to send the command to.

------------------------------------------------------------------------

### `Chromote$invoke_event_callbacks()`

Immediately call all event callback methods.

For internal use only.

#### Usage

    Chromote$invoke_event_callbacks(event, params)

#### Arguments

- `event`:

  A single event string

- `params`:

  A list of parameters to pass to the event callback methods.

------------------------------------------------------------------------

### `Chromote$debug_messages()`

Enable or disable message debugging

If enabled, R will print out the

#### Usage

    Chromote$debug_messages(value = NULL)

#### Arguments

- `value`:

  If `TRUE`, enable debugging. If `FALSE`, disable debugging.

------------------------------------------------------------------------

### `Chromote$debug_log()`

Submit debug log message

#### Examples

    b <- ChromoteSession$new()
    b$parent$debug_messages(TRUE)
    b$Page$navigate("https://www.r-project.org/")
    #> SEND {"method":"Page.navigate","params":{"url":"https://www.r-project.org/"}| __truncated__}
    # Turn off debug messages
    b$parent$debug_messages(FALSE)

#### Usage

    Chromote$debug_log(...)

#### Arguments

- `...`:

  Arguments pasted together with `paste0(..., collapse = "")`.

------------------------------------------------------------------------

### `Chromote$url()`

Create url for a given path

#### Usage

    Chromote$url(path = NULL)

#### Arguments

- `path`:

  A path string to append to the host and port

------------------------------------------------------------------------

### `Chromote$is_active()`

Is there an active websocket connection to the browser process?

#### Usage

    Chromote$is_active()

------------------------------------------------------------------------

### `Chromote$is_alive()`

Is the underlying browser process running?

#### Usage

    Chromote$is_alive()

------------------------------------------------------------------------

### `Chromote$check_active()`

Check that a chromote instance is active and alive. Will automatically
reconnect if browser process is alive, but there's no active web socket
connection.

#### Usage

    Chromote$check_active()

------------------------------------------------------------------------

### `Chromote$get_browser()`

Retrieve
[`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)\`
object

#### Usage

    Chromote$get_browser()

------------------------------------------------------------------------

### `Chromote$close()`

Close the
[`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)
object

#### Usage

    Chromote$close(wait = TRUE)

#### Arguments

- `wait`:

  If an integer, waits a number of seconds for the process to exit,
  killing the process if it takes longer than `wait` seconds to close.
  Use `wait = TRUE` to wait for 10 seconds, or `wait = FALSE` to close
  the connection without waiting for the process to exit. Only applies
  when Chromote is connected to a local process.

------------------------------------------------------------------------

### `Chromote$print()`

Summarise the current state of the object.

#### Usage

    Chromote$print(..., verbose = FALSE)

#### Arguments

- `...`:

  Passed on to [`format()`](https://rdrr.io/r/base/format.html) when
  `verbose` = TRUE

- `verbose`:

  The print method defaults to a brief summary of the most important
  debugging info; use `verbose = TRUE` tp see the complex R6 object.
