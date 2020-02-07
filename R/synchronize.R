promise_globals <- new.env(parent = emptyenv())
promise_globals$interrupt_domains <- list()

push_interrupt_domain <- function(domain) {
  n_domains <- length(promise_globals$interrupt_domains)
  promise_globals$interrupt_domains[[n_domains + 1]] <- domain
}

pop_interrupt_domain <- function() {
  n_domains <- length(promise_globals$interrupt_domains)
  if (length(n_domains) == 0)
    return(NULL)

  domain <- promise_globals$interrupt_domains[[n_domains]]
  promise_globals$interrupt_domains[[n_domains]] <- NULL

  domain
}

current_interrupt_domain <- function() {
  if (length(promise_globals$interrupt_domains) == 0) {
    return(NULL)
  }

  promise_globals$interrupt_domains[[length(promise_globals$interrupt_domains)]]
}


create_interrupt_domain <- function() {
  domain <- new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      function(...) {
        push_interrupt_domain(domain)
        on.exit(pop_interrupt_domain(), add = TRUE)

        if (domain$interrupted) {
          stop("Operation was interrupted 1")
        }
        tryCatch(
          {
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
      function(...) {
        push_interrupt_domain(domain)
        on.exit(pop_interrupt_domain(), add = TRUE)

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
      function(...) {
        push_interrupt_domain(domain)
        on.exit(pop_interrupt_domain(), add = TRUE)

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
      push_interrupt_domain(domain)
      on.exit(pop_interrupt_domain(), add = TRUE)

      # Counting is currently not used
      if (is.null(promise_globals$synchronized)) {
        promise_globals$synchronized <- 0L
      }
      promise_globals$synchronized <- promise_globals$synchronized + 1L
      on.exit(promise_globals$synchronized <- promise_globals$synchronized - 1L, add = TRUE)

      force(expr)
    },
    interrupted = FALSE
  )

  domain
}


# This function takes a promise and blocks until it is resolved. It runs the
# promise's callbacks in the provided event loop. If the promise is
# interrupted then this function tries to catch the interrupt, then runs the
# loop until it's empty; then it throws a new interrupt. If the promise throws
# an error, then also throws an error.
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
        generateInterrupt()
      }
    )
  })
}


# A wrapper for later() which polls for interrupts. If an interrupt has
# occurred either while running another callback, or when run_now() is
# waiting, the interrupt will be detected and (1) the scheduled `func` will be
# cancelled, and (2) the `on_interrupt` callback will be invoked.
later_with_interrupt <- function(
  func,
  delay = 0,
  loop = current_loop(),
  on_interrupt = function() {},
  interrupt_domain = current_interrupt_domain(),
  poll_interval = 0.1
) {
  force(func)
  force(loop)
  force(interrupt_domain)
  force(on_interrupt)
  force(poll_interval)

  if (is.null(interrupt_domain)) {
    return(later(func, delay, loop))
  }

  end_time <- as.numeric(Sys.time()) + delay
  cancel <- NULL

  nextTurn <- function(init = FALSE) {
    if (isTRUE(interrupt_domain$interrupted)) {
      on_interrupt()
      return()
    }

    this_delay <- min(poll_interval, end_time - as.numeric(Sys.time()))
    if (this_delay <= 0) {
      # Time has expired. If we've never progressed to the next tick (i.e.
      # init == TRUE) then don't invoke the callback yet, wait until the next
      # tick. Otherwise, do invoke the callback.
      if (!init) {
        func()
        return()
      }
      this_delay <- 0
    }
    cancel <<- later::later(nextTurn, this_delay, loop)
  }
  nextTurn(init = TRUE)

  function() {
    cancel()
  }
}
# TODO
#
# The notion of cancellability should go into later package. Instead of this
# function taking `interrupt_domain`, which is a promise-level object, we could
# do something like the following:
#
# Add on_interrupt option to later(); if FALSE/NULL (the default) then interrupts
#   don't affect scheduled callback. If TRUE, then interrupt cancels the later().
#   If a function, then interrupt cancels the later() and calls the on_interrupt
#   function.
# Add later::interrupt() function, so that later can know that an interrupt
#   happened.
# Add option to later() to make the callback uninterruptable.



generateInterrupt <- function() {
  tools::pskill(Sys.getpid(), tools::SIGINT)
  Sys.sleep(1)
}
