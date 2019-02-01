#' @import rlang

globalVariables("private")

process_protocol <- function(protocol, env = parent.frame()) {
  message("Protocol version: ", protocol$version$major, ".", protocol$version$minor)

  domains <- protocol$domains
  names(domains) <- vapply(domains, function(d) d$domain, "")
  domains <- lapply(domains, function(domain) {
    commands <- get_commands(domain)
    lapply(commands, command_to_function, domain_name = domain$domain, env = env)
  })

  domains
}

get_commands <- function(domain) {
  commands <- domain$commands
  if (is.null(commands)) {
    return(list())
  } else {
    names(commands) <- fetch_key_c(commands, "name")
    commands
  }
}

command_to_function <- function(command, domain_name, env) {
  rlang::new_function(
    args = gen_args(command$parameters),
    body = gen_body(paste0(domain_name, ".", command$name), command$parameters),
    env  = env
  )
  # TODO:
  # * Add type-checking
  # * Cross-reference types for type checking
}

gen_args <- function(params) {
  args <- lapply(params, function(param) {
    if (!isTRUE(param$optional)) {
      missing_arg()
    } else {
      NULL
    }
  })

  names(args) <- fetch_key_c(params, "name")
  args
}


# Returns a function body.
# method_name is something like "Browser.getVersion"
gen_body <- function(method_name, params) {

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
    private$send(msg)
  })
}
