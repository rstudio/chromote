.onUnload <- function(libpath) {
  # Shut down browser
  if (!is.null(globals$process) && globals$process$is_alive()) {
    globals$process$signal(tools::SIGTERM)
  }
}
