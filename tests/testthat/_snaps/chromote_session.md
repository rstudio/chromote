# ChromoteSession auto_events_enable_args errors

    Code
      ChromoteSession$new(auto_events_enable_args = NULL)
    Condition
      Error in `initialize()`:
      ! `auto_events_enable_args` must be a named list of domains and associated arguments for the `enable` command.

---

    Code
      ChromoteSession$new(auto_events_enable_args = list("also bad"))
    Condition
      Error in `initialize()`:
      ! `auto_events_enable_args` must be a named list of domains and associated arguments for the `enable` command.

---

    Code
      ChromoteSession$new(auto_events_enable_args = list(Browser = list(no_enable = TRUE)))
    Condition
      Error in `self$auto_events_enable_args()`:
      ! Browser does not have an enable method.

---

    Code
      ChromoteSession$new(auto_events_enable_args = list(Animation = list(bad = TRUE)))
    Condition
      Error in `self$auto_events_enable_args()`:
      ! Animation.enable does not have argument: `bad`.
      i Available arguments: `callback_`, `error_`, and `timeout_`

