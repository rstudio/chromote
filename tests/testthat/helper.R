skip_if_no_chromote <- function() {
  skip_on_cran()
  skip_if(lacks_chromote(), "chromote not available")
}

lacks_chromote <- function() {
  # We try twice because in particular Windows on GHA seems to need it,
  # but it doesn't otherwise hurt. More details at
  # https://github.com/rstudio/shinytest2/issues/209
  env_cache(globals, "lacks_chromote", !has_chromote() && !has_chromote())
}

has_chromote <- function() {
  tryCatch(
    {
      default <- default_chromote_object()
      local_bindings(default_timeout = 5, .env = default)
      startup <- default$new_session(wait_ = FALSE)
      default$wait_for(startup)
      TRUE
    },
    error = function(cnd) {
      FALSE
    }
  )
}

with_retries <- function(fn, max_tries = 3) {
  trace <- trace_back()

  retry <- function(tried = 0) {
    tryCatch(
      {
        fn()
      },
      error = function(err) {
        tried <- tried + 1
        if (tried >= max_tries) {
          rlang::abort(
            sprintf("Failed after %s tries", tried),
            parent = err,
            trace = trace
          )
        } else {
          retry(tried)
        }
      }
    )
  }

  retry()
}
