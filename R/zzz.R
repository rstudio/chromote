`_dummy_` <- function() {
  # Make a dummy curl call to make R CMD check happy
  # {jsonlite} only suggests {curl}, but is needed for standard {chromote} usage
  # https://github.com/rstudio/chromote/issues/37
  curl::curl

  invisible()
}
