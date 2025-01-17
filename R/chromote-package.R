#' @keywords internal
"_PACKAGE"

#' chromote Options
#'
#' @description
#' These options and environment variables that are used by chromote. Options
#' are lowercase and can be set with `options()`. Environment variables are
#' uppercase and can be set in an `.Renviron` file, with `Sys.setenv()`, or in
#' the shell or process running R. If both an option or environment variable are
#' supported, chromote will use the option first.
#'
#' * `CHROMOTE_CHROME` \cr
#'   Path to the Chrome executable. If not set, chromote will
#'   attempt to find and use the system installation of Chrome.
#' * `chromote.headless`, `CHROMOTE_HEADLESS` \cr
#'   Headless mode for Chrome. Can be `"old"` or `"new"`. See
#'   [Chrome Headless mode](https://developer.chrome.com/docs/chromium/new-headless)
#'   for more details.
#' * `chromote.timeout` \cr
#'   Timeout (in seconds) for Chrome to launch or connect. Default is `10`.
#' * `chromote.launch.echo_cmd` \cr
#'   Echo the command used to launch Chrome to the console for debugging.
#'   Default is `FALSE`.
#'
#' @name chromote-options
NULL

## usethis namespace: start
#' @import promises later
#' @importFrom fastmap fastmap
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom processx process
#' @importFrom R6 R6Class
#' @importFrom websocket WebSocket
## usethis namespace: end
NULL
