#' @import rlang

globalVariables("private")

process_protocol <- function(protocol, env = parent.frame()) {
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
  rlang::new_function(
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
  args <- c(args, callback_ = list(NULL), error_ = list(NULL),
            timeout_ = quote(self$default_timeout))
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
    # Check for missing non-optional args
    !!!check_missing_exprs

    msg <- list(
      method = !!method_name,
      params = drop_nulls(list(!!!param_list))
    )
    private$send_command(msg, callback = callback_, error = error_, timeout = timeout_)
  })
}



event_to_function <- function(event, domain_name, env) {
  rlang::new_function(
    args = list(callback_ = NULL, timeout_ = quote(self$default_timeout)),
    body = gen_event_body(paste0(domain_name, ".", event$name)),
    env  = env
  )
}

# Returns a function body for registering an event callback.
# method_name is something like "Page.loadEventFired".
gen_event_body <- function(method_name) {
  expr({
    private$register_event_listener(!!method_name, callback_, timeout_)
  })
}
