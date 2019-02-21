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
