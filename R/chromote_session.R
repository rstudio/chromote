ChromoteSession <- R6Class("ChromoteSession",
  lock_objects = FALSE,
  public = list(
    initialize = function(parent, protocol, session_id) {
      private$parent     <- parent
      private$session_id <- session_id

      self$protocol <- wrap_protocol(protocol, env = self$.__enclos_env__)
      # Graft the entries from self$protocol onto self
      list2env(self$protocol, self)
    },

    get_session_id = function() {
      private$session_id
    },

    protocol = NULL
  ),

  private = list(
    parent = NULL,
    session_id = NULL
  )
)


wrap_protocol <- function(protocol, env) {
  domain_names <- names(protocol)
  protocol_wrapped <- mapply(
    FUN = wrap_domain,
    domain = protocol,
    domain_name = domain_names,
    MoreArgs = list(env = env)
  )
}


wrap_domain <- function(domain, domain_name, env) {
  method_names <- setNames(names(domain), names(domain))
  lapply(method_names, wrap_method, domain_name = domain_name, env = env)
}


wrap_method <- function(method_name, domain_name, env) {
  fn <- function(...) {
    method_name(..., sessionId_ = private$session_id)
  }

  body(fn) <- expr({
    # Equivalent to:
    # private$parent$protocol$(!!domain_name)$(!!method_name)(..., sessionId_ = private$session_id)
    # except that the syntax above isn't valid -- we need to write in the
    # verbose way with `$`(). When printed at the console, it'll be shown in
    # the nicer format.
    `$`(
      `$`(
        private$parent$protocol,
        !!domain_name
      ),
      !!method_name
    )(..., sessionId_ = private$session_id)
  })

  environment(fn) <- env

  fn
}
