EventManager <- R6Class(
  "EventManager",
  public = list(
    initialize = function(session) {
      private$session <- session

      if (length(session$protocol) == 0) {
        stop("Session object must have non-empty protocol field.")
      }

      # Find out which domains require the <domain>.enable command to enable
      # event notifications.
      private$event_enable_domains <- lapply(
        session$protocol,
        function(domain) {
          is.function(domain$enable)
        }
      )

      private$event_callbacks <- fastmap()
    },

    register_event_listener = function(event, callback = NULL, timeout = NULL) {
      domain <- find_domain(event)

      # Note: If callback is specified, then timeout is ignored. Also, returns
      # a function for deregistering the callback, instead of a promise.
      if (!is.null(callback)) {
        deregister_callback_fn <- private$add_event_callback(
          event,
          callback,
          once = FALSE
        )
        return(invisible(deregister_callback_fn))
      }

      deregister_callback_fn <- NULL
      p <- promise(function(resolve, reject) {
        deregister_callback_fn <<- private$add_event_callback(
          event,
          resolve,
          once = TRUE
        )
      })

      if (!is.null(timeout) && !is.infinite(timeout)) {
        # !!! TODO: Fix loop !!!
        p <- promise_timeout(
          p,
          timeout,
          loop = private$session$get_child_loop(),
          timeout_message = paste0(
            "Chromote: timed out waiting for event ",
            event
          )
        )
      }

      p <- p$finally(function() {
        deregister_callback_fn()
      })
      p
    },

    invoke_event_callbacks = function(event, params) {
      callbacks <- private$event_callbacks$get(event)
      if (is.null(callbacks) || callbacks$size() == 0) return()

      callbacks$invoke(params)
    },

    remove_event_callbacks = function(event) {
      # Removes ALL callbacks for a given event. In the future it might be
      # useful to implement finer control.
      private$event_callbacks$remove(event)
    }
  ),

  private = list(
    # The ChromoteSession or Chromote object that owns this EventManager.
    session = NULL,
    event_callbacks = NULL,
    # For keeping count of the number of callbacks for each domain; if
    # auto_events is TRUE, then when the count goes from 0 to 1 or 1 to 0 for
    # a given domain, it will automatically enable or disable events for that
    # domain.
    event_callback_counts = list(),

    # Some domains require a <domain>.event command to enable event
    # notifications, others do not. (Not really sure why.)
    event_enable_domains = NULL,

    add_event_callback = function(event, callback, once) {
      if (!private$event_callbacks$has(event)) {
        private$event_callbacks$set(event, Callbacks$new())
      }

      if (once) {
        orig_callback <- callback
        callback <- function(...) {
          tryCatch(
            orig_callback(...),
            finally = deregister_and_dec()
          )
        }
      }

      deregister_callback <- private$event_callbacks$get(event)$add(callback)

      domain <- find_domain(event)
      private$inc_event_callback_count(domain)

      # We'll wrap deregister_callback in another function which also keeps
      # count to the number of callbacks for the domain.
      deregister_called <- FALSE
      deregister_and_dec <- function() {
        # Make sure that if this is called multiple times that it doesn't keep
        # having effects.
        if (deregister_called) return()
        deregister_called <<- TRUE

        deregister_callback()
        private$dec_event_callback_count(domain)
      }

      deregister_and_dec
    },

    inc_event_callback_count = function(domain) {
      if (is.null(private$event_callback_counts[[domain]])) {
        private$event_callback_counts[[domain]] <- 0
      }

      private$event_callback_counts[[domain]] <-
        private$event_callback_counts[[domain]] + 1

      private$session$debug_log(
        "Callbacks for ",
        domain,
        "++: ",
        private$event_callback_counts[[domain]]
      )

      # If we're doing auto events and we're going from 0 to 1, enable events
      # for this domain. (Some domains do not require or have an .enable
      # method.)
      if (
        private$session$get_auto_events() &&
          private$event_callback_counts[[domain]] == 1 &&
          isTRUE(private$event_enable_domains[[domain]])
      ) {
        private$session$debug_log("Enabling events for ", domain)
        private$session[[domain]]$enable()
      }

      invisible(private$event_callback_counts[[domain]])
    },

    dec_event_callback_count = function(domain) {
      private$event_callback_counts[[domain]] <-
        private$event_callback_counts[[domain]] - 1

      private$session$debug_log(
        "Callbacks for ",
        domain,
        "--: ",
        private$event_callback_counts[[domain]]
      )
      # If we're doing auto events and we're going from 1 to 0, disable
      # enable events for this domain.
      if (
        private$session$get_auto_events() &&
          private$event_callback_counts[[domain]] == 0 &&
          isTRUE(private$event_enable_domains[[domain]])
      ) {
        private$session$debug_log("Disabling events for ", domain)
        private$session[[domain]]$disable()
      }

      invisible(private$event_callback_counts[[domain]])
    }
  )
)
