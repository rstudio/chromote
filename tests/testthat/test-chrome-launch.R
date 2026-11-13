test_that("launch_chrome() stops port search when the OS cannot spawn Chrome", {
  attempts <- 0
  original <- rlang::catch_cnd(
    rlang::abort(
      "Cannot spawn process (system error 2)",
      class = "c_error_test"
    )
  )

  fake_process <- list(
    new = function(...) {
      attempts <<- attempts + 1
      rlang::cnd_signal(original)
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")

  err <- expect_error(
    launch_chrome("/path/to/missing-chrome", args = character()),
    class = "error_stop_port_search"
  )

  expect_equal(attempts, 1)
  expect_false(rlang::cnd_inherits(err, "error_no_available_port"))
  expect_identical(err$parent, original)
  expect_match(conditionMessage(err), "/path/to/missing-chrome", fixed = TRUE)
  expect_match(conditionMessage(err), "could not launch", fixed = TRUE)
})

test_that("launch_chrome() passes bare command names to processx unchanged", {
  commands <- character()
  fake_process <- list(
    new = function(command, ...) {
      commands <<- c(commands, command)
      stop("spawn failed")
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")

  expect_error(
    launch_chrome("chrome-not-on-path", args = character()),
    class = "error_stop_port_search"
  )
  expect_identical(commands, "chrome-not-on-path")
})

test_that("launch_chrome() stops after one spawn attempt with real processx", {
  # processx fails in `process$new()` before any process is created, so this
  # does not launch Chrome. The directory exists only for this test and the
  # executable inside it is absent.
  attempts <- 0
  real_process <- processx::process
  fake_process <- list(
    new = function(...) {
      attempts <<- attempts + 1
      real_process$new(...)
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")

  missing <- file.path(withr::local_tempdir(), "chrome")
  err <- expect_error(
    launch_chrome(missing, args = character()),
    class = "error_stop_port_search"
  )

  expect_equal(attempts, 1)
  expect_false(rlang::cnd_inherits(err, "error_no_available_port"))
  expect_false(is.null(err$parent))
  expect_match(conditionMessage(err), missing, fixed = TRUE)
})

test_that("launch_chrome() still retries other ports for retryable errors", {
  attempts <- 0
  log_dir <- withr::local_tempdir()
  fake_process <- list(
    new = function(...) {
      attempts <<- attempts + 1
      # Process starts but dies before exposing the debugging port, which is
      # a generic error that should be retried on a new port.
      error_file <- tempfile(tmpdir = log_dir)
      output_file <- tempfile(tmpdir = log_dir)
      # Chrome reports an in-use port with this message and exits without
      # ever serving the debugging port.
      writeLines(
        "[ERROR:devtools_http_handler.cc] bind() returned an error: Address already in use",
        error_file
      )
      file.create(output_file)
      list(
        is_alive = function() FALSE,
        get_error_file = function() error_file,
        get_output_file = function() output_file
      )
    }
  )
  local_mocked_bindings(process = fake_process, .package = "chromote")
  local_mocked_bindings(
    chrome_verify = function(...) list(status = 1),
    .package = "chromote"
  )
  withr::local_options(chromote.timeout = 1)

  expect_error(
    launch_chrome("/path/to/chrome", args = character()),
    class = "error_no_available_port"
  )
  expect_equal(attempts, 10)
})

test_that("launch_chrome() stops the port search when the browser relaunched itself", {
  skip_on_cran()
  # The fake relauncher below is a shell script
  skip_on_os("windows")
  skip_if(
    !exists("serverSocket", "package:base"),
    "serverSocket() not available"
  )

  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if(!file.exists(rscript), "Rscript not found")

  # A fake DevTools server: serves /json/version like a real browser.
  server_script <- tempfile("chromote-fake-devtools-", fileext = ".R")
  writeLines(
    c(
      "port <- as.integer(commandArgs(trailingOnly = TRUE)[[1]])",
      "srv <- serverSocket(port)",
      "body <- '{\"Browser\": \"FakeRelaunchedBrowser/1.0\"}'",
      "repeat {",
      "  con <- socketAccept(srv, blocking = FALSE, open = 'r+b')",
      "  req <- raw(0)",
      "  # Consume the request before responding, so closing the connection",
      "  # doesn't reset it before the client can read the response.",
      "  deadline <- Sys.time() + 5",
      "  repeat {",
      "    chunk <- readBin(con, 'raw', n = 65536)",
      "    if (length(chunk)) req <- c(req, chunk)",
      "    if (grepl('\\r\\n\\r\\n', rawToChar(req), fixed = TRUE)) break",
      "    if (Sys.time() > deadline) break",
      "    Sys.sleep(0.02)",
      "  }",
      "  writeBin(charToRaw(paste0(",
      "    'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n',",
      "    'Content-Length: ', length(charToRaw(body)), '\r\n',",
      "    'Connection: close\r\n\r\n', body",
      "  )), con)",
      "  flush(con)",
      "  Sys.sleep(0.05)",
      "  close(con)",
      "}"
    ),
    server_script
  )

  # A fake browser that behaves like Microsoft Edge on Windows (#243): it
  # passes the debugging port to a child process and exits, leaving the port
  # served by the child. It is named "msedge" so that the error carries the
  # Edge-specific advice.
  pidfile <- tempfile("chromote-fake-devtools-pid-")
  relauncher <- file.path(withr::local_tempdir(), "msedge")
  writeLines(
    c(
      "#!/bin/sh",
      "for arg in \"$@\"; do",
      "  case \"$arg\" in",
      "    --remote-debugging-port=*) port=\"${arg#*=}\" ;;",
      "  esac",
      "done",
      sprintf('"%s" --vanilla "%s" "$port" &', rscript, server_script),
      sprintf('echo $! > "%s"', pidfile),
      "exit 0"
    ),
    relauncher
  )
  Sys.chmod(relauncher, "0755")
  withr::defer(tryCatch(
    tools::pskill(as.integer(readLines(pidfile))),
    error = function(e) NULL
  ))

  err <- expect_error(
    launch_chrome(relauncher, args = character()),
    class = "error_browser_relaunch"
  )
  expect_false(rlang::cnd_inherits(err, "error_no_available_port"))

  msg <- paste(err$message, collapse = "\n")
  expect_match(msg, "relaunched itself", fixed = TRUE)
  # Browser-specific advice, keyed by the executable name.
  expect_match(msg, "--edge-skip-compat-layer-relaunch", fixed = TRUE)
})
