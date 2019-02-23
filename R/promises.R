promise_timeout <- function(p, timeout, loop = current_loop(),
                            timeout_message = NULL)
{
  promise(function(resolve, reject) {
    timer <- later(function() {
      if (is.null(timeout_message)) {
        timeout_message <- "Promise timed out"
      }

      reject(timeout_message)
    }, timeout, loop = loop)

    p$then(
      onFulfilled = function(value) {
        # TODO: Clear timer to free memory. Will require changes to later.
        resolve(value)
      },
      onRejected = function(err) {
        # TODO: Clear timer to free memory
        reject(err)
      }
    )
  })
}



# More convenient way of chaining together promises than then/catch/finally,
# without the performance impact of %...>%.
promise_chain <- function(p, ..., catch = NULL, finally = NULL) {
  for (fn in list(...)) {
    p <- p$then(fn)
  }

  if (!is.null(catch)) {
    p <- p$catch(catch)
  }

  if (!is.null(finally)) {
    p <- p$finally(finally)
  }

  p
}


hybrid_chain <- function(expr, ..., catch = NULL, finally = NULL) {
  runFinally <- TRUE
  tryCatch(
    {
      result <- force(expr)

      if (promises::is.promising(result)) {
        p <- promise_chain(result, ..., catch = catch, finally = finally)
        runFinally <- FALSE
        invisible(p)

      } else {
        for (fn in list(...)) {
          result <- fn(result)
        }
      }
    },
    error = function(e) {
      if (!is.null(catch))
        catch(e)
      else
        stop(e)
    },
    finally = {
      if (runFinally && !is.null(finally)) finally()
    }
  )
}
