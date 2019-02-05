# Wait for websocket connection to open. This should be used after calling
# ws$connect().
ws_poll_until_connected <- function(ws, timeout = 5) {
  connected <- FALSE
  end <- Sys.time() + timeout
  while (!connected && Sys.time() < end) {
    # Need to run the event loop for websocket to complete connection.
    later::run_now(0.1)

    ready_state <- ws$readyState()
    if (ready_state == 0L) {
      # 0 means we're still trying to connect. Do nothing.
    } else if (ready_state == 1L) {
      connected <- TRUE
    } else {
      break
    }
  }

  if (!connected) {
    stop("Unable to establish websocket connection.")
  }
}
