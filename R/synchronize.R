promise_globals <- new.env(parent = emptyenv())

# TODO:
# Maintain a current interrupt domain global variable (package level)

create_interrupt_domain <- function() {
  domain <- new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      # message("wrapOnFulfilled")
      function(...) {
        # TODO: store previous interrupt domain, set new interrupt domain, on
        # exit restore previous.
        # message("wrapOnFulfilled inner")
        if (domain$interrupted) {
          stop("Operation was interrupted 1")
        }
        tryCatch(
          {
            # message("wrapOnFulfilled inner tryCatch")
            onFulfilled(...)
          },
          interrupt = function(e) {
            # message("wrapOnFulfilled inner caught interrupt")
            # Call function here that returns current interrupt
            domain$interrupted <- TRUE
            stop("Operation was interrupted 2")
          }
        )
      }
    },
    wrapOnRejected = function(onRejected) {
      # message("wrapOnRejected")
      function(...) {
        # TODO: store previous interrupt domain, set new interrupt domain, on
        # exit restore previous.
        # message("wrapOnRejected inner")
        if (domain$interrupted) {
          stop("Operation was interrupted 3")
        }
        tryCatch(
          onRejected(...),
          interrupt = function(e) {
            domain$interrupted <- TRUE
            stop("Operation was interrupted 4")
          }
        )
      }
    },
    wrapOnFinally = function(onFinally) {
      # message("wrapOnFinally")
      function(...) {
        # TODO: store previous interrupt domain, set new interrupt domain, on
        # exit restore previous.
        # message("wrapOnFinally inner")
        tryCatch(
          onFinally(...),
          interrupt = function(e) {
            domain$interrupted <- TRUE
            stop("Operation was interrupted 5")
          }
        )
      }
    },
    wrapSync = function(expr) {
      # TODO: store previous interrupt domain, set new interrupt domain, on
      # exit restore previous.
      # message("wrapSync")

      # Counting is currently not used
      if (is.null(promise_globals$synchronized)) {
        promise_globals$synchronized <- 0L
      }
      promise_globals$synchronized <- promise_globals$synchronized + 1L
      on.exit(promise_globals$synchronized <- promise_globals$synchronized - 1L)

      force(expr)
    },
    interrupted = FALSE
  )

  domain
}

synchronize <- function(expr, loop) {
  domain <- create_interrupt_domain()

  with_promise_domain(domain, {
    tryCatch(
      {
        result <- force(expr)

        if (is.promising(result)) {
          value <- NULL
          type <- NULL
          result$
            then(function(val) {
              value <<- val
              type <<- "success"
            })$
            catch(function(reason) {
              value <<- reason
              type <<- "error"
            })

          while (is.null(type) && !domain$interrupted) {
            run_now(loop = loop)
          }

          if (is.null(type)) {
            generateInterrupt()
          } else if (type == "success") {
            value
          } else if (type == "error") {
            stop(value)
          }
        }
      },
      interrupt = function(e) {
        domain$interrupted <<- TRUE
        message("Attempting to interrupt gracefully; press Esc/Ctrl+C to force interrupt")
        while (!loop_empty(loop = loop)) {
          run_now(loop = loop)
        }
        # TODO: This needs to change to something that actually works (SIGINT?)
        message("generateInterrupt")
        generateInterrupt()
        message("generateInterrupt done")
      }
    )
  })
}

generateInterrupt <- function() {
  # TODO: Do something that actually works
  tools::pskill(Sys.getpid(), tools::SIGINT)
  Sys.sleep(1)

  # stop(structure(list(), class = c("interrupt", "condition")))
}
