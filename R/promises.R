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
    cancel_timer <- later_with_interrupt(
      function() {
        if (is.null(timeout_message)) {
          timeout_message <- "Promise timed out"
        }

        reject(timeout_message)
      },
      timeout,
      loop = loop,
      on_interrupt = function() {
        reject("interrupted")
      }
    )

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
