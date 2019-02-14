.onUnload <- function(libpath) {
  # Shut down browser
  if (!is.null(globals$chrome)) {
    globals$chrome$close()
  }
}
