#' @importFrom promises %...>%
#' @export
promises::"%...>%"

#' @importFrom promises %...!%
#' @export
promises::"%...!%"

#' @importFrom promises %...T>%
#' @export
promises::"%...T>%"

#' @importFrom promises %...T!%
#' @export
promises::"%...T!%"

#' @importFrom magrittr %>%
#' @export
magrittr::"%>%"

#' @importFrom magrittr %T>%
#' @export
magrittr::"%T>%"



#' @importFrom promises promise
#' @export
promises::promise

#' @importFrom promises then
#' @export
promises::then

#' @importFrom promises catch
#' @export
promises::catch

#' @importFrom promises finally
#' @export
promises::finally

promise_timeout <- function(p, timeout, loop = current_loop(),
                            timeout_message = NULL)
{
  promise(function(resolve, reject) {
    cancel_timer <- later(function() {
      if (is.null(timeout_message)) {
        timeout_message <- "Promise timed out"
      }

      reject(timeout_message)
    }, timeout, loop = loop)

    p$then(
      onFulfilled = function(value) {
        # Timer is no longer needed, so we'll cancel it to free memory.
        cancel_timer()
        resolve(value)
      },
      onRejected = function(err) {
        cancel_timer()
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
      result <- withVisible(force(expr))

      if (promises::is.promising(result$value)) {
        p <- promise_chain(setVisible(result), ..., catch = catch, finally = finally)
        runFinally <- FALSE
        invisible(p)

      } else {
        for (fn in list(...)) {
          if (".visible" %in% names(formals(fn))) {
            result <- withVisible(fn(result$value, .visible = result$visible))
          } else {
            result <- withVisible(fn(result$value))
          }
        }
        setVisible(result)
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

# Returns `value` with either `invisible()` applied or not, depending on the
# value of `visible`.
#
# If the `visible` is missing, then `value` should be a list as returned from
# `withVisible()`, and that visibility will be applied.
setVisible <- function(value, visible) {
  if (missing(visible)) {
    visible <- value$visible
    value <- value$value
  }

  if (!visible) {
    invisible(value)
  } else {
    value
  }
}
