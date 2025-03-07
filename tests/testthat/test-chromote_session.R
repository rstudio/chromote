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
