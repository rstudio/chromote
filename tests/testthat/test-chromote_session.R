test_that("respawning preserves targetId and auto_events", {
  skip_if_no_chromote()

  sess1 <- create_session(auto_events = FALSE)
  sess2 <- sess1$respawn()

  expect_equal(sess1$get_target_id(), sess2$get_target_id())
  expect_equal(sess1$get_auto_events(), sess2$get_auto_events())
})

test_that("ChromoteSession track metrics from `Emulation.setDeviceMetricsOverride`", {
  skip_if_no_chromote()

  page <- ChromoteSession$new(mobile = TRUE)
  withr::defer(page$close())

  expect_true(page$.__enclos_env__$private$is_mobile)

  page$Emulation$setDeviceMetricsOverride(
    600,
    600,
    deviceScaleFactor = 2,
    mobile = FALSE
  )
  expect_false(page$.__enclos_env__$private$is_mobile)
  expect_equal(page$.__enclos_env__$private$pixel_ratio, 2)
})

test_that("ChromoteSession gets and sets viewport size", {
  skip_if_no_chromote()
  skip_if_offline()

  page <- ChromoteSession$new(width = 400, height = 800, mobile = TRUE)
  # viewport requires an active page
  page$Page$navigate("https://example.com")
  withr::defer(page$close())

  init_size <- list(
    width = 400,
    height = 800,
    zoom = page$.__enclos_env__$private$pixel_ratio,
    mobile = TRUE
  )

  expect_equal(
    page$get_viewport_size(),
    init_size
  )

  expect_equal(
    page$set_viewport_size(500, 900, zoom = 2, mobile = FALSE),
    init_size # returned invisibly
  )

  expect_equal(
    page$get_viewport_size(),
    list(
      width = 500,
      height = 900,
      zoom = 2,
      mobile = FALSE
    )
  )
})

test_that("ChromoteSession inherits `auto_events_enable_args` from parent", {
  skip_if_no_chromote()

  args <- list(
    Fetch = list(handleAuthRequests = TRUE),
    Network = list(maxTotalBufferSize = 1024)
  )

  parent <- Chromote$new(auto_events_enable_args = args)
  page <- ChromoteSession$new(parent = parent)

  expect_equal(
    page$auto_events_enable_args("Fetch"),
    !!args[["Fetch"]]
  )

  expect_equal(
    page$auto_events_enable_args("Network"),
    !!args[["Network"]]
  )

  page$auto_events_enable_args("Fetch", handleAuthRequests = FALSE)
  expect_equal(
    page$auto_events_enable_args("Fetch"),
    list(handleAuthRequests = FALSE)
  )
  expect_equal(
    parent$auto_events_enable_args("Fetch"),
    !!args[["Fetch"]]
  )
})

test_that("ChromoteSession$new(auto_events_enable_args)", {
  skip_if_no_chromote()

  # b <- ChromoteSession$new()
  # ls(b, pattern = "^[A-Z]") |>
  #   set_names() |>
  #   lapply(\(p) if (is_function(b[[p]]$enable)) names(fn_fmls(b[[p]]$enable))) |>
  #   purrr::compact() |>
  #   str()

  args_parent <- list(DOM = list(includeWhitespace = FALSE))
  args_page <- list(DOM = list(includeWhitespace = TRUE))

  parent <- Chromote$new(auto_events_enable_args = args_parent)
  page <- ChromoteSession$new(
    parent = parent,
    auto_events_enable_args = args_page
  )

  expect_equal(
    page$auto_events_enable_args("DOM"),
    !!args_page[["DOM"]]
  )

  expect_equal(
    page$parent$auto_events_enable_args("DOM"),
    !!args_parent[["DOM"]]
  )

  # Unset local page-specific auto events args
  page$auto_events_enable_args("DOM", NULL)
  expect_equal(
    page$auto_events_enable_args("DOM"),
    !!args_parent[["DOM"]]
  )
})

test_that("ChromoteSession auto_events_enable_args errors", {
  skip_if_no_chromote()

  expect_snapshot(
    ChromoteSession$new(auto_events_enable_args = NULL),
    error = TRUE
  )

  expect_snapshot(
    ChromoteSession$new(auto_events_enable_args = list("also bad")),
    error = TRUE
  )

  expect_snapshot(
    ChromoteSession$new(
      auto_events_enable_args = list(Browser = list(no_enable = TRUE))
    ),
    error = TRUE
  )

  expect_snapshot(
    ChromoteSession$new(
      auto_events_enable_args = list(Animation = list(bad = TRUE))
    ),
    error = TRUE
  )

  expect_warning(
    ChromoteSession$new(
      auto_events_enable_args = list(Animation = list(wait_ = TRUE))
    )
  )
})
