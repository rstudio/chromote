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

test_that("ChromoteSession with deviceScaleFactor = 0", {
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
    page$set_viewport_size(500, 900, zoom = 0, mobile = FALSE),
    init_size # returned invisibly
  )

  expect_null(page$.__enclos_env__$private$pixel_ratio)

  expect_equal(
    page$get_viewport_size(),
    list(
      width = 500,
      height = 900,
      zoom = 0,
      mobile = FALSE
    )
  )
})
