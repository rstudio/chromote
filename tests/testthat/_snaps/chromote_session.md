# ChromoteSession auto_events_enable_args errors

    Code
      chromote_session$auto_events_enable_args("Browser", no_enable = TRUE)
    Condition
      Error in `chromote_session$auto_events_enable_args()`:
      ! Browser does not have an enable method.

---

    Code
      chromote_session$auto_events_enable_args("Animation", bad = TRUE)
    Condition
      Error in `chromote_session$auto_events_enable_args()`:
      ! Animation.enable does not have argument: `bad`.
      i Available arguments: `callback_`, `error_`, and `timeout_`

