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
#' @import promises later rlang
#' @importFrom fastmap fastmap
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom processx process
#' @importFrom R6 R6Class
#' @importFrom websocket WebSocket
## usethis namespace: end
NULL

# inlined from `lifecycle::badge()` and only supports the experimental badge.
# Use `usethis::use_lifecycle()` to add additional badges.
lifecycle_badge <- function(stage) {
  stage <- rlang::arg_match0(
    stage,
    c("experimental") #, "stable", "superseded", "deprecated")
  )
  stage_name <- substr(stage, 1, 1) <- toupper(substr(stage, 1, 1))

  url <- paste0("https://lifecycle.r-lib.org/articles/stages.html#", stage)

  html <- sprintf(
    "\\href{%s}{\\figure{%s}{options: alt='[%s]'}}",
    url,
    file.path(tolower(sprintf("lifecycle-%s.svg", stage))),
    stage_name
  )

  text <- sprintf("\\strong{[%s]}", stage_name)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}
