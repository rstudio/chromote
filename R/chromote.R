#' Chromote class
#'
#' @description
#' A `Chromote` object represents the browser as a whole, and it can have
#' multiple _targets_, which each represent a browser tab. In the Chrome
#' DevTools Protocol, each target can have one or more debugging _sessions_ to
#' control it. A `ChromoteSession` object represents a single _session_.
#'
#' A `Chromote` object can have any number of `ChromoteSession` objects as
#' children. It is not necessary to create a `Chromote` object manually. You can
#' simply call:
#' ```r
#' b <- ChromoteSession$new()
#' ```
#' and it will automatically create a `Chromote` object if one has not already
#' been created. The \pkg{chromote} package will then designate that `Chromote`
#' object as the _default_ `Chromote` object for the package, so that any future
#' calls to `ChromoteSession$new()` will automatically use the same `Chromote`.
#' This is so that it doesn't start a new browser for every `ChromoteSession`
#' object that is created.
#' @export
Chromote <- R6Class(
  "Chromote",
  lock_objects = FALSE,
  cloneable = FALSE,
  public = list(
    #' @param browser A [`Browser`] object
    #' @param multi_session Should multiple sessions be allowed?
    #' @param auto_events If `TRUE`, enable automatic event enabling/disabling;
    #'   if `FALSE`, disable automatic event enabling/disabling.
    initialize = function(
      browser = Chrome$new(),
      multi_session = TRUE,
      auto_events = TRUE
    ) {
      private$browser <- browser
      private$auto_events <- auto_events
      private$multi_session <- multi_session

      private$command_callbacks <- fastmap()

      # Use a private event loop to drive the websocket
      private$child_loop <- create_loop(parent = current_loop())

      p <- self$connect(multi_session = multi_session, wait_ = FALSE)

      # Populate methods while the connection is being established.
      protocol_spec <- jsonlite::fromJSON(
        self$url("/json/protocol"),
        simplifyVector = FALSE
      )
      self$protocol <- process_protocol(protocol_spec, self$.__enclos_env__)
      lockBinding("protocol", self)
      # self$protocol is a list of domains, each of which is a list of
      # methods. Graft the entries from self$protocol onto self
      list2env(self$protocol, self)

      private$event_manager <- EventManager$new(self)

      self$wait_for(p)

      private$register_default_event_listeners()
    },

    #' @description Re-connect the websocket to the browser. The Chrome browser
    #'   automatically closes websockets when your computer goes to sleep;
    #'   you can use this to bring it back to life with a new connection.
    #' @param multi_session Should multiple sessions be allowed?
    #' @param wait_ If `FALSE`, return a promise; if `TRUE` wait until
    #'   connection is complete.
    connect = function(multi_session = TRUE, wait_ = TRUE) {
      if (multi_session) {
        chrome_info <- fromJSON(self$url("/json/version"))
      } else {
        chrome_info <- fromJSON(self$url("/json"))
      }

      with_loop(private$child_loop, {
        private$ws <- WebSocket$new(
          chrome_info$webSocketDebuggerUrl,
          autoConnect = FALSE
        )

        private$ws$onMessage(private$on_message)

        # Allow up to 10 seconds to connect to browser.

        # TODO: The extra promise_resolve()$then() wrapper is currently
        # necessary because promise_timeout needs to be run _within_ a
        # synchronize() call (which $wait_for(), down below, does). If we call
        # promise_timeout() directly here, then it will error out because
        # there isn't a current interrupt domain. Hopefully we can remove this
        # delay and extra wrapper stuff.
        p <- promise_resolve(TRUE)$then(function(value) {
          promise_timeout(
            promise(function(resolve, reject) {
              private$ws$onOpen(resolve)
            }),
            timeout = getOption("chromote.timeout", 10),
            timeout_message = paste0(
              "Chromote: timed out waiting for WebSocket connection to browser. ",
              "Use `options(chromote.timeout = ",
              getOption("chromote.timeout", 10),
              ")` ",
              "to increase the timeout."
            )
          )
        })

        private$ws$connect()
      })

      if (wait_) {
        invisible(self$wait_for(p))
      } else {
        p
      }
    },

    #' @description Display the current session in the `browser`
    #'
    #' If a [`Chrome`] browser is being used, this method will open a new tab
    #' using your [`Chrome`] browser. When not using a [`Chrome`] browser, set
    #' `options(browser=)` to change the default behavior of [`browseURL()`].
    view = function() {
      browse_url(path = NULL, self)
    },

    #' @description
    #' `auto_events` value.
    #'
    #' For internal use only.
    get_auto_events = function() {
      private$auto_events
    },

    # =========================================================================
    # Event loop, promises, and synchronization
    # =========================================================================

    #' @description Local \pkg{later} loop.
    #'
    #' For expert async usage only.
    get_child_loop = function() {
      private$child_loop
    },

    # This runs the child loop until the promise is resolved.
    #' @description Wait until the promise resolves
    #'
    #' Blocks the R session until the promise (`p`) is resolved. The loop from
    #' `$get_child_loop()` will only advance just far enough for the promise to
    #' resolve.
    #' @param p A promise to resolve.
    wait_for = function(p) {
      if (!is.promise(p)) {
        stop("wait_for requires a promise object.")
      }

      synchronize(p, loop = private$child_loop)
    },

    # =========================================================================
    # Session management
    # =========================================================================

    #' @description Create a new tab / window
    #'
    #' @param width,height Width and height of the new window.
    #' @param targetId
    #'   [Target](https://chromedevtools.github.io/devtools-protocol/tot/Target/)
    #'   ID of an existing target to attach to. When a `targetId` is provided, the
    #'   `width` and `height` arguments are ignored. If NULL (the default) a new
    #'   target is created and attached to, and the `width` and `height`
    #'   arguments determine its viewport size.
    #' @param wait_ If `FALSE`, return a [promises::promise()] of a new
    #'   `ChromoteSession` object. Otherwise, block during initialization, and
    #'   return a `ChromoteSession` object directly.
    new_session = function(
      width = 992,
      height = 1323,
      targetId = NULL,
      wait_ = TRUE
    ) {
      self$check_active()
      create_session(
        chromote = self,
        width = width,
        height = height,
        targetId = targetId,
        wait_ = wait_
      )
    },

    #' @description Retrieve all [`ChromoteSession`] objects
    #' @return A list of `ChromoteSession` objects
    get_sessions = function() {
      private$sessions
    },

    #' @description Register [`ChromoteSession`] object
    #' @param session A `ChromoteSession` object
    #'
    #' For internal use only.
    register_session = function(session) {
      private$sessions[[session$get_session_id()]] <- session
    },

    # =========================================================================
    # Commands and events
    # =========================================================================

    #' @description
    #' Send command through Chrome DevTools Protocol.
    #'
    #' For expert use only.
    #' @param msg A JSON-serializable list containing `method`, and `params`.
    #' @param callback Method to run when the command finishes successfully.
    #' @param error Method to run if an error occurs.
    #' @param timeout Number of milliseconds for Chrome DevTools Protocol
    #' execute a method.
    #' @param sessionId Determines which [`ChromoteSession`] with the
    #' corresponding to send the command to.
    send_command = function(
      msg,
      callback = NULL,
      error = NULL,
      timeout = NULL,
      sessionId = NULL
    ) {
      self$check_active()

      private$last_msg_id <- private$last_msg_id + 1
      msg$id <- private$last_msg_id

      if (!is.null(sessionId)) {
        msg$sessionId <- sessionId
      }

      p <- promise(function(resolve, reject) {
        msg_json <- toJSON(msg, auto_unbox = TRUE)
        private$ws$send(msg_json)
        self$debug_log("SEND ", msg_json)
        # One of these callbacks will be invoked when a message arrives with a
        # matching id.
        private$add_command_callback(msg$id, resolve, reject)
      })

      p <- p$catch(function(e) {
        stop(
          "code: ",
          e$code,
          "\n  message: ",
          e$message,
          if (!is.null(e$data)) paste0("\n  data: ", e$data)
        )
      })

      if (!is.null(timeout) && !is.infinite(timeout)) {
        p <- promise_timeout(
          p,
          timeout,
          loop = private$child_loop,
          timeout_message = paste0(
            "Chromote: timed out waiting for response to command ",
            msg$method
          )
        )
      }

      if (!is.null(callback)) {
        p <- p$then(onFulfilled = callback, onRejected = error)
      }

      p <- p$finally(function() private$remove_command_callback(msg$id))

      p
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

    # =========================================================================
    # Debugging
    # =========================================================================

    #' @description Enable or disable message debugging
    #'
    #' If enabled, R will print out the
    # JSON messages that are sent and received. If called with no value, this
    # method will print out the current debugging state.
    #' @param value If `TRUE`, enable debugging. If `FALSE`, disable debugging.
    debug_messages = function(value = NULL) {
      if (is.null(value)) return(private$debug_messages_)

      if (!(identical(value, TRUE) || identical(value, FALSE)))
        stop("value must be TRUE or FALSE")

      private$debug_messages_ <- value
    },

    #' @description
    #' Submit debug log message
    #'
    #' ## Examples
    #'
    #' ```r
    #' b <- ChromoteSession$new()
    #' b$parent$debug_messages(TRUE)
    #' b$Page$navigate("https://www.r-project.org/")
    #' #> SEND {"method":"Page.navigate","params":{"url":"https://www.r-project.org/"}| __truncated__}
    #' # Turn off debug messages
    #' b$parent$debug_messages(FALSE)
    #' ```
    #'
    #' @param ... Arguments pasted together with `paste0(..., collapse = "")`.
    debug_log = function(...) {
      txt <- truncate(paste0(..., collapse = ""), 1000)
      if (private$debug_messages_) {
        message(txt)
      }
    },

    # =========================================================================
    # Misc utility functions
    # =========================================================================

    #' @description Create url for a given path
    #' @param path A path string to append to the host and port
    url = function(path = NULL) {
      if (!is.null(path) && substr(path, 1, 1) != "/") {
        stop('path must be NULL or a string that starts with "/"')
      }
      paste0(
        "http://",
        private$browser$get_host(),
        ":",
        private$browser$get_port(),
        path
      )
    },

    #' @description
    #' Is there an active websocket connection to the browser process?
    is_active = function() {
      self$is_alive() && private$ws$readyState() %in% c(0L, 1L)
    },

    #' @description
    #' Is the underlying browser process running?
    is_alive = function() {
      private$browser$is_alive()
    },

    #' @description Check that a chromote instance is active and alive.
    #'  Will automatically reconnect if browser process is alive, but
    #'  there's no active web socket connection.
    check_active = function() {
      if (!self$is_alive()) {
        stop("Chromote has been closed.")
      }

      if (!self$is_active()) {
        inform(
          c(
            "!" = "Reconnecting to chrome process.",
            i = "All active sessions will be need to be respawned."
          )
        )
        self$connect()

        # Mark all sessions as closed
        for (session in private$sessions) {
          session$mark_closed(FALSE)
        }
        private$sessions <- list()
      }
      invisible(self)
    },

    #' @description Retrieve [`Browser`]` object
    #'
    get_browser = function() {
      private$browser
    },

    #' @description Close the [`Browser`] object
    close = function() {
      # Must be alive to be active so we cache value before closing process
      is_active <- self$is_active()

      if (self$is_alive()) {
        if (is_active) {
          # send a message to the browser requesting that it close
          self$Browser$close()
        } else {
          # terminate the process
          private$browser$close()
        }
      }

      if (is_active) {
        private$ws$close()
      }

      invisible()
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
          state <- "active + alive"
        } else if (self$is_alive()) {
          state <- "alive"
        } else {
          state <- "closed"
        }

        ps <- self$get_browser()$get_process()

        cat_line("<Chromote> (", state, ")")
        if (self$is_alive()) {
          cat_line("  URL:  ", self$url())
          cat_line("  PID:  ", ps$get_pid())
          cat_line("  Path: ", ps$get_cmdline()[[1]])
        }
      }
      invisible(self)
    },

    #' @field default_timeout Default timeout in seconds for \pkg{chromote} to
    #' wait for a Chrome DevTools Protocol response.
    default_timeout = 10,
    #' @field protocol Dynamic protocol implementation. For expert use only!
    protocol = NULL
  ),

  private = list(
    browser = NULL,
    ws = NULL,

    # =========================================================================
    # Browser commands
    # =========================================================================
    last_msg_id = 0,
    command_callbacks = NULL,

    add_command_callback = function(id, callback, error) {
      id <- as.character(id)
      private$command_callbacks$set(
        id,
        list(
          callback = callback,
          error = error
        )
      )
    },

    # Invoke the callback for a command (using id).
    invoke_command_callback = function(id, value, error) {
      id <- as.character(id)

      if (!private$command_callbacks$has(id)) return()

      handlers <- private$command_callbacks$get(id)

      if (!is.null(error)) {
        handlers$error(error)
      } else if (!is.null(value)) {
        handlers$callback(value)
      }
    },

    remove_command_callback = function(id) {
      private$command_callbacks$remove(as.character(id))
    },

    # =========================================================================
    # Browser events
    # =========================================================================
    event_manager = NULL,

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      self$check_active()
      private$event_manager$register_event_listener(event, callback, timeout)
    },

    register_default_event_listeners = function() {
      # When a target is closed, mark the corresponding R session object as
      # closed and remove it from the list of sessions.
      self$protocol$Target$detachedFromTarget(function(msg) {
        sid <- msg$sessionId
        session <- private$sessions[[sid]]
        if (is.null(session)) return()

        private$sessions[[sid]] <- NULL
        session$mark_closed(TRUE)
      })
    },

    # =========================================================================
    # Message handling and dispatch
    # =========================================================================
    debug_messages_ = FALSE,
    debug_message_max_length = 1000,

    on_message = function(msg) {
      self$debug_log("RECV ", msg$data)
      data <- fromJSON(msg$data, simplifyVector = FALSE)

      if (!is.null(data$method)) {
        # This is an event notification.
        #
        # The reason that the callback is wrapped in later() is to prevent a
        # possible race when a command response and an event notification arrive
        # in the same tick. See issue #1.
        later(function() {
          if (!is.null(data$sessionId)) {
            session <- private$sessions[[data$sessionId]]
          } else {
            session <- self
          }

          session$invoke_event_callbacks(data$method, data$params)
        })
      } else if (!is.null(data$id)) {
        # This is a response to a command.
        private$invoke_command_callback(data$id, data$result, data$error)
      } else {
        message("Don't know how to handle message: ", msg$data)
      }
    },

    # =========================================================================
    # Sessions
    # =========================================================================
    multi_session = NULL,
    sessions = list(),

    # =========================================================================
    # Private event loop for the websocket
    # =========================================================================
    child_loop = NULL
  )
)

globals$default_chromote <- NULL

#' Default Chromote object
#'
#' Returns the Chromote package's default \code{\link{Chromote}} object. If
#' there is not currently a default \code{Chromote} object that is active, then
#' one will be created and set as the default.
#'
#' \code{\link{ChromoteSession}$new()} calls this function by default, if the
#' \code{parent} is not specified. That means that when
#' \code{\link{ChromoteSession}$new()} is called and there is not currently an
#' active default \code{Chromote} object, then a new \code{Chromote} object will
#' be created and set as the default.
#' @export
default_chromote_object <- function() {
  if (!has_default_chromote_object()) {
    set_default_chromote_object(Chromote$new())
  }

  globals$default_chromote
}

#' Returns TRUE if there's a default Chromote object and it is active, FALSE
#' otherwise.
#' @rdname default_chromote_object
#' @export
has_default_chromote_object <- function() {
  !is.null(globals$default_chromote) && globals$default_chromote$is_alive()
}

#' @param x A \code{\link{Chromote}} object.
#' @rdname default_chromote_object
#' @export
set_default_chromote_object <- function(x) {
  if (!inherits(x, "Chromote")) {
    stop("x must be a Chromote object.")
  }
  globals$default_chromote <- x
}

cache_value <- function(fn) {
  value <- NULL
  function() {
    if (is.null(value)) {
      value <<- fn()
    }
    value
  }
}
# inspired by https://www.npmjs.com/package/is-docker
# This should not change over time. Cache it
is_inside_docker <- cache_value(function() {
  file.exists("/.dockerenv") ||
    (
      is_linux() &&
        file.exists("/proc/self/cgroup") &&
        any(grepl("docker", readLines("/proc/self/cgroup"), fixed = TRUE))
    )
})

# This is a _fast_ function. Do not cache it.
is_inside_ci <- function() {
  !identical(Sys.getenv("CI", unset = ""), "")
}

is_missing_linux_user <- cache_value(function() {
  is_linux() &&
    system("id", ignore.stdout = TRUE) != 0
})

#' Default Chrome arguments
#'
#' A character vector of command-line arguments passed when initializing any new
#' instance of [`Chrome`]. Single on-off arguments are passed as single values
#' (e.g.`"--disable-gpu"`), arguments with a value are given with a nested
#' character vector (e.g. `c("--force-color-profile", "srgb")`). See
#' [here](https://peter.sh/experiments/chromium-command-line-switches/) for a
#' list of possible arguments.
#'
#'
#' @details
#'
#' Default chromote arguments are composed of the following values (when
#' appropriate):
#'
#' * [`"--disable-gpu"`](https://peter.sh/experiments/chromium-command-line-switches/#disable-gpu)
#'   * Only added on Windows, as empirically it appears to be needed
#'     (if not, check runs on GHA never terminate).
#'   * Disables GPU hardware acceleration. If software renderer is not in place, then the GPU process won't launch.
#' * [`"--no-sandbox"`](https://peter.sh/experiments/chromium-command-line-switches/#no-sandbox)
#'   * Only added when `CI` system environment variable is set, when the
#'     user on a Linux system is not set, or when executing inside a Docker container.
#'   * Disables the sandbox for all process types that are normally sandboxed. Meant to be used as a browser-level switch for testing purposes only
#' * [`"--disable-dev-shm-usage"`](https://peter.sh/experiments/chromium-command-line-switches/#disable-dev-shm-usage)
#'   * Only added when `CI` system environment variable is set or when inside a docker instance.
#'   * The `/dev/shm` partition is too small in certain VM environments, causing Chrome to fail or crash.
#' * [`"--force-color-profile=srgb"`](https://peter.sh/experiments/chromium-command-line-switches/#force-color-profile)
#'   * This means that screenshots taken on a laptop plugged into an external
#'     monitor will often have subtly different colors than one taken when
#'     the laptop is using its built-in monitor. This problem will be even
#'     more likely across machines.
#'   * Force all monitors to be treated as though they have the specified color profile.
#' * [`"--disable-extensions"`](https://peter.sh/experiments/chromium-command-line-switches/#disable-extensions)
#'   * Disable extensions.
#' * [`"--mute-audio"`](https://peter.sh/experiments/chromium-command-line-switches/#mute-audio)
#'   * Mutes audio sent to the audio device so it is not audible during automated testing.
#'
#' @return A character vector of default command-line arguments to be used with
#'   every new [`ChromoteSession`]
#' @describeIn default_chrome_args Returns a character vector of command-line
#'   arguments passed when initializing Chrome. See Details for more
#'   information.
#' @export
default_chrome_args <- function() {
  c(
    # Empirically, appears to be needed for check runs to terminate on GHA
    if (is_windows()) "--disable-gpu",

    # > Note: --no-sandbox is not needed if you properly setup a user in the container.
    # https://developers.google.com/web/updates/2017/04/headless-chrome
    if (is_inside_ci() || is_missing_linux_user() || is_inside_docker()) {
      "--no-sandbox"
    },

    # Until we have hundreds of concurrent usage, let's slow things down by
    # using `/tmp` disk folder, rather than shared memory folder `/dev/shm`.
    # This will make things more stable at the cost of accessing disk more often.
    # Great discussion: https://github.com/puppeteer/puppeteer/issues/1834
    if (is_inside_ci() || is_inside_docker()) {
      "--disable-dev-shm-usage" # required bc the target easily crashes
    },

    # Consistent screenshot colors
    # https://github.com/rstudio/chromote/pull/52
    "--force-color-profile=srgb",

    # Have also seen usage of `--ignore-certificate-errors`

    # Generic options to have consistent output
    c(
      '--disable-extensions',
      '--mute-audio'
    )
  )
}

#' @describeIn default_chrome_args Retrieves the default command-line arguments
#'   passed to [`Chrome`] during initialization. Returns either `NULL` or a
#'   character vector.
#' @export
get_chrome_args <- function() {
  if (!exists("chrome_args", envir = globals)) {
    set_chrome_args(default_chrome_args())
  }

  globals$chrome_args
}
reset_chrome_args <- function() {
  rm("chrome_args", envir = globals)
}

#' @describeIn default_chrome_args Sets the default command-line arguments
#'   passed when initializing. Returns the updated defaults.
#' @param args A character vector of command-line arguments (or `NULL`) to be
#'   used with every new [`ChromoteSession`].
#' @export
#' @examples
#' old_chrome_args <- get_chrome_args()
#'
#' # Disable the gpu and use of `/dev/shm`
#' set_chrome_args(c("--disable-gpu", "--disable-dev-shm-usage"))
#'
#' #... Make new `Chrome` or `ChromoteSession` instance
#'
#' # Restore old defaults
#' set_chrome_args(old_chrome_args)
set_chrome_args <- function(args) {
  set_args <- function(args_) {
    # Using $ to set `NULL` is safe within environments
    globals$chrome_args <- args_
    invisible(args_)
  }

  # Validate
  default_args <- unique(unlist(args))
  if (length(default_args) == 0) {
    return(set_args(NULL))
  }
  if (
    anyNA(default_args) || !any(vapply(default_args, is.character, logical(1)))
  ) {
    stop("`set_chrome_args()` only accepts a character vector or `NULL`")
  }

  # Set
  return(set_args(default_args))
}
