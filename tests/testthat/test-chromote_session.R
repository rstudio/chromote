test_that("respawning preserves targetId and auto_events", {
  skip_if_no_chromote()

  sess1 <- create_session(auto_events = FALSE)
  sess2 <- sess1$respawn()

  expect_equal(sess1$get_target_id(), sess2$get_target_id())
  expect_equal(sess1$get_auto_events(), sess2$get_auto_events())
})
