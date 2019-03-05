#' @import rlang

globalVariables("private")

# Given a protocol spec (essentially, the Chrome Devtools Protocol JSON
# converted to an R object), returns a list of domains of the Devtools
# Protocol (like Browser, Page, Runtime). Each domain has a function for each
# command and event (like Browser$getVersion, Page$navigate, etc). The
# `protocol` input is the protocol object from the browser, translated from
# JSON to an R object, and the `env` is the desired environment that is
# assigned to the the generated functions -- it should be the Chromote
# object's enclosing environment so that the functions can find `self` and
# `private`.
process_protocol <- function(protocol, env) {
  message("Protocol version: ", protocol$version$major, ".", protocol$version$minor)

  domains <- protocol$domains
  names(domains) <- vapply(domains, function(d) d$domain, "")
  domains <- lapply(domains, function(domain) {
    commands <- get_items(domain, "commands")
    commands <- lapply(commands, command_to_function, domain_name = domain$domain, env = env)

    events <- get_items(domain, "events")
    events <- lapply(events, event_to_function, domain_name = domain$domain, env = env)

    c(commands, events)
  })

  domains
}

# Returns commands or events for a given domain
get_items <- function(domain, type = c("commands", "events")) {
  type <- match.arg(type)
  methods <- domain[[type]]
  if (is.null(methods)) {
    return(list())
  } else {
    names(methods) <- fetch_key_c(methods, "name")
    methods
  }
}

command_to_function <- function(command, domain_name, env) {
  new_function(
    args = gen_command_args(command$parameters),
    body = gen_command_body(paste0(domain_name, ".", command$name), command$parameters),
    env  = env
  )
  # TODO:
  # * Add type-checking
  # * Cross-reference types for type checking
}

gen_command_args <- function(params) {
  args <- lapply(params, function(param) {
    if (!isTRUE(param$optional)) {
      missing_arg()
    } else {
      NULL
    }
  })

  names(args) <- fetch_key_c(params, "name")
  args <- c(
    args,
    callback_ = list(NULL),
    error_ = list(NULL),
    timeout_ = quote(self$default_timeout)
  )
  args
}


# Returns a function body for a command.
# method_name is something like "Browser.getVersion"
gen_command_body <- function(method_name, params) {

  # Construct expressions for checking missing args
  required_params <- params[!fetch_key_l(params, "optional", default = FALSE)]
  check_missing_exprs <- lapply(required_params, function(param) {
    name <- as.symbol(param$name)
    check_missing <- expr(
      if (missing(!!name)) stop("Missing required argument ", !!(expr_text(name)))
    )
  })

  # Construct parameters for message
  param_list <- lapply(params, function(param) {
    as.symbol(param$name)
  })
  names(param_list) <- fetch_key_c(params, "name")

  expr({
    if (!is.null(callback_) && !is.function(callback_))
      stop("`callback_` must be a function or NULL.")

    if (!is.null(error_) && !is.function(error_))
      stop("`error_` must be a function or NULL.")

    if (!is.null(timeout_) && !is.numeric(timeout_))
      stop("`timeout_` must be a number or NULL.")


    # Check for missing non-optional args
    !!!check_missing_exprs

    msg <- list(
      method = !!method_name,
      params = drop_nulls(list(!!!param_list))
    )
    self$send_command(
      msg,
      callback = callback_,
      error = error_,
      timeout = timeout_
    )
  })
}



event_to_function <- function(event, domain_name, env) {
  new_function(
    args = list(
      callback_ = NULL,
      timeout_ = quote(self$default_timeout)
    ),
    body = gen_event_body(paste0(domain_name, ".", event$name)),
    env  = env
  )
}

# Returns a function body for registering an event callback.
# method_name is something like "Page.loadEventFired".
gen_event_body <- function(method_name) {
  expr({
    if (!is.null(callback_) && !is.function(callback_))
      stop("`callback_` must be a function or NULL.")

    if (!is.null(timeout_) && !is.numeric(timeout_))
      stop("`timeout_` must be a number or NULL.")

    private$register_event_listener(!!method_name, callback_, timeout_)
  })
}



# Given a protocol object, reassign the environment for all functions.
protocol_reassign_envs <- function(protocol, env) {
  lapply(protocol, function(domain) {
    lapply(domain, function(method) {
      environment(method) <- env
      method
    })
  })
}
