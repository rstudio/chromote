test_version <- "156.0.8078.12"

# Offline cache rooted at `root`. The download is stubbed, and the default stub
# fails so that any unexpected network access fails the test.
local_chrome_cache <- function(root, download = NULL, env = parent.frame()) {
  download <- download %||% function(url, path) stop("Unexpected download")
  versions <- data.frame(
    version = test_version,
    revision = "1",
    binary = "chrome",
    platform = c("linux64", "win64", "mac-arm64"),
    url = "https://example.invalid/chrome.zip"
  )

  local_mocked_bindings(
    chromote_cache_path = function(...) file.path(root, ...),
    chrome_get_versions = function(update_cached = TRUE) versions,
    chrome_download_file = download,
    .package = "chromote",
    .env = env
  )
  invisible(root)
}

install_chrome <- function(binary = "chrome", platform = "linux64") {
  chrome_versions_ensure(test_version, binary = binary, platform = platform)
}

# `chrome_path_conflict()` reports paths from dirname(), which uses `/`
# separators on Windows, so expected paths are built the same way
walked_path <- function(path) dirname(file.path(path, "."))

write_fake_file <- function(path, content = "fake", exec = TRUE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(content), path)
  Sys.chmod(path, if (exec) "755" else "644")
  invisible(path)
}

write_fake_zip <- function(zip_path, entries) {
  src <- withr::local_tempdir()
  for (rel in names(entries)) {
    write_fake_file(file.path(src, rel), entries[[rel]])
  }
  zip::zip(zip_path, files = names(entries), root = src, mode = "mirror")
  invisible(zip_path)
}

# Asserts a rejected cache entry: the error reason, that the entry is left in
# place, and that recovery guidance includes an exact unlink() of `dir`. Remove
# is never the only recovery path, because it skips installs with no executable.
expect_cache_invalid <- function(
  reason,
  dir,
  binary = "chrome",
  platform = "linux64"
) {
  err <- expect_error(
    install_chrome(binary = binary, platform = platform),
    class = "error_chrome_invalid_binary"
  )
  expect_equal(err$reason, reason)
  expect_equal(err$version, test_version)
  expect_equal(err$platform, platform)
  expect_true(chrome_path_exists(dir))

  expect_match(err$remove_cmd, "chrome_versions_remove(", fixed = TRUE)
  expect_match(err$add_cmd, "chrome_versions_add(", fixed = TRUE)
  expect_equal(
    err$unlink_cmd,
    paste0("unlink(", encodeString(dir, quote = '"'), ", recursive = TRUE)")
  )
  invisible(err)
}

expect_no_staging_files <- function(vdir) {
  leftovers <- list.files(
    vdir,
    pattern = "^\\.(download|staging)-",
    all.files = TRUE,
    no.. = TRUE
  )
  expect_length(leftovers, 0)
}

test_that("a valid cached binary is reused without downloading", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  exe <- file.path(root, "chrome", test_version, "chrome-linux64", "chrome")
  write_fake_file(exe)

  expect_equal(install_chrome()$path, exe)
})

test_that("a symlinked executable inside a bundle is accepted", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  local_chrome_cache(root)

  target <- file.path(root, "resources", "Google Chrome for Testing")
  write_fake_file(target)

  macos_dir <- file.path(
    root,
    "chrome",
    test_version,
    "chrome-mac-arm64",
    "Google Chrome for Testing.app",
    "Contents",
    "MacOS"
  )
  dir.create(macos_dir, recursive = TRUE)
  exe <- file.path(macos_dir, "Google Chrome for Testing")
  expect_true(file.symlink(target, exe))

  expect_equal(install_chrome(platform = "mac-arm64")$path, exe)
})

test_that("absent paths are not reported as broken symlinks", {
  root <- withr::local_tempdir()
  absent <- file.path(root, "absent")

  expect_false(chrome_is_broken_symlink(absent))
  expect_false(chrome_path_exists(absent))
})

test_that("a fresh install into an empty cache is not blocked by absent paths", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) {
    write_fake_zip(path, c("chrome-linux64/chrome" = "fake chrome"))
  })

  result <- install_chrome()

  expect_equal(result$path, file.path(vdir, "chrome-linux64", "chrome"))
  expect_true(utils::file_test("-f", result$path))
  expect_no_staging_files(vdir)
})

test_that("an empty cached binary is rejected without downloading", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  dir <- file.path(root, "chrome", test_version, "chrome-linux64")
  write_fake_file(file.path(dir, "chrome"), content = "")

  expect_cache_invalid("empty", dir = dir)
})

test_that("a cached directory at the binary path is rejected without downloading", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  dir <- file.path(root, "chrome", test_version, "chrome-linux64")
  dir.create(file.path(dir, "chrome"), recursive = TRUE)

  expect_cache_invalid("a directory, not a file", dir = dir)
})

test_that("a cached broken symlink is rejected without downloading", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  local_chrome_cache(root)

  dir <- file.path(root, "chrome", test_version, "chrome-linux64")
  dir.create(dir, recursive = TRUE)
  expect_true(file.symlink(
    file.path(root, "does-not-exist"),
    file.path(dir, "chrome")
  ))

  expect_cache_invalid("broken symlink", dir = dir)
})

test_that("a missing executable in an existing platform directory is incomplete", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  dir <- file.path(root, "chrome", test_version, "chrome-linux64")
  write_fake_file(file.path(dir, "resources.pak"))

  expect_cache_invalid(
    "incomplete (executable missing from existing directory)",
    dir = dir
  )
  expect_true(file.exists(file.path(dir, "resources.pak")))
})

test_that("a file where the platform directory should be is a blocking conflict", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  dir <- file.path(root, "chrome", test_version, "chrome-linux64")
  write_fake_file(dir)

  expect_cache_invalid(
    sprintf("expected directory %s is a file", walked_path(dir)),
    dir = dir
  )
})

test_that("a file where a bundle directory should be recovers the whole bundle", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  bundle <- file.path(root, "chrome", test_version, "chrome-mac-arm64")
  app <- file.path(bundle, "Google Chrome for Testing.app")
  write_fake_file(app)

  expect_cache_invalid(
    sprintf("expected directory %s is a file", walked_path(app)),
    dir = bundle,
    platform = "mac-arm64"
  )
})

test_that("a file replacing a nested Contents directory recovers the whole bundle", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)

  bundle <- file.path(root, "chrome", test_version, "chrome-mac-arm64")
  app <- file.path(bundle, "Google Chrome for Testing.app")
  contents <- file.path(app, "Contents")
  dir.create(app, recursive = TRUE)
  write_fake_file(contents)

  expect_cache_invalid(
    sprintf("expected directory %s is a file", walked_path(contents)),
    dir = bundle,
    platform = "mac-arm64"
  )
})

test_that("the blocker walk stops at the cache root, not above it", {
  root <- withr::local_tempdir()
  blocker <- file.path(root, "blocker")
  write_fake_file(blocker, exec = FALSE)

  # `blocker` is a file above the cache root, so it must never be reported
  cache <- file.path(blocker, "cache")
  path <- file.path(cache, "chrome", test_version, "chrome-linux64", "chrome")
  expect_null(chrome_path_conflict(path, paste0(cache, "/")))
  expect_null(chrome_path_conflict(path, cache))
})

test_that("a Windows cache root with backslashes stops the blocker walk", {
  skip_on_os(c("mac", "linux", "solaris"))

  root <- withr::local_tempdir()
  blocker <- file.path(root, "blocker")
  write_fake_file(blocker, exec = FALSE)

  cache <- gsub("/", "\\\\", file.path(blocker, "cache"))
  path <- file.path(cache, "chrome", test_version, "chrome-linux64", "chrome")
  expect_null(chrome_path_conflict(path, cache))
})

test_that("a broken symlink nested in the bundle recovers the whole bundle", {
  skip_on_os("windows")

  root <- withr::local_tempdir()
  local_chrome_cache(root)

  bundle <- file.path(root, "chrome", test_version, "chrome-mac-arm64")
  macos <- file.path(
    bundle,
    "Google Chrome for Testing.app",
    "Contents",
    "MacOS"
  )
  dir.create(dirname(macos), recursive = TRUE)
  expect_true(file.symlink(file.path(root, "missing"), macos))

  expect_cache_invalid(
    sprintf("expected directory %s is a broken symlink", macos),
    dir = bundle,
    platform = "mac-arm64"
  )
})

test_that("Unix non-executable binaries are rejected", {
  skip_if_not(chrome_binary_checks_exec())

  root <- withr::local_tempdir()
  local_chrome_cache(root)

  dir <- file.path(root, "chrome", test_version, "chrome-linux64")
  exe <- write_fake_file(file.path(dir, "chrome"), exec = FALSE)

  expect_cache_invalid("not executable", dir = dir)
  expect_true(file.access(exe, 1) != 0)
})

test_that("the Unix execute check is skipped for Windows binaries", {
  root <- withr::local_tempdir()
  local_chrome_cache(root)
  local_mocked_bindings(
    chrome_binary_checks_exec = function() FALSE,
    .package = "chromote"
  )

  exe <- file.path(root, "chrome", test_version, "chrome-win64", "chrome.exe")
  write_fake_file(exe, exec = FALSE)

  expect_equal(install_chrome(platform = "win64")$path, exe)
})

test_that("recovery commands are valid R for Windows and quoted paths", {
  dir <- 'C:\\Users\\me\\My "Chrome"\\chrome-win64'
  resolved <- list(
    path = file.path(dir, "chrome.exe"),
    version = test_version,
    binary = "chrome",
    platform = "win64"
  )

  err <- expect_error(
    chrome_abort_invalid_binary("empty", resolved, dir = dir),
    class = "error_chrome_invalid_binary"
  )

  expect_equal(
    err$unlink_cmd,
    'unlink("C:\\\\Users\\\\me\\\\My \\"Chrome\\"\\\\chrome-win64", recursive = TRUE)'
  )
  expect_equal(eval(parse(text = err$unlink_cmd)[[1]][[2]]), dir)
  expect_no_error(parse(text = err$remove_cmd))
})

test_that("a successful install publishes only the platform subtree", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) {
    write_fake_zip(
      path,
      c(
        "chrome-linux64/chrome" = "fake chrome",
        "chrome-linux64/resources.pak" = "pak"
      )
    )
  })

  sibling <- file.path(
    vdir,
    "chrome-headless-shell-linux64",
    "chrome-headless-shell"
  )
  write_fake_file(sibling, "sibling")

  result <- install_chrome()

  expect_equal(result$path, file.path(vdir, "chrome-linux64", "chrome"))
  expect_true(utils::file_test("-f", result$path))
  expect_true(file.exists(file.path(vdir, "chrome-linux64", "resources.pak")))
  expect_equal(readBin(sibling, "raw", 100), charToRaw("sibling"))
  expect_setequal(
    list.files(vdir, all.files = TRUE, no.. = TRUE),
    c("chrome-headless-shell-linux64", "chrome-linux64")
  )
  expect_no_staging_files(vdir)
})

test_that("Windows setup runs on the staged binary before publication", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  final_dir <- file.path(vdir, "chrome-win64")
  local_chrome_cache(root, download = function(url, path) {
    write_fake_zip(
      path,
      c(
        "chrome-win64/chrome.exe" = "fake chrome",
        "chrome-win64/setup.exe" = "fake setup"
      )
    )
  })

  setup_call <- NULL
  local_mocked_bindings(
    chrome_install_windows_run_setup = function(path) {
      setup_call <<- list(
        path = path,
        final_exists = dir.exists(final_dir),
        setup_exists = file.exists(file.path(dirname(path), "setup.exe"))
      )
    },
    chrome_binary_checks_exec = function() FALSE,
    .package = "chromote"
  )

  result <- install_chrome(platform = "win64")

  expect_match(setup_call$path, ".staging-", fixed = TRUE)
  expect_false(setup_call$final_exists)
  expect_true(setup_call$setup_exists)
  expect_equal(result$path, file.path(final_dir, "chrome.exe"))
  expect_true(file.exists(file.path(final_dir, "setup.exe")))
  expect_no_staging_files(vdir)
})

test_that("a refused publish with no concurrent winner fails explicitly", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) {
    write_fake_zip(path, c("chrome-linux64/chrome" = "fake chrome"))
  })
  local_mocked_bindings(
    chrome_rename_dir = function(from, to) FALSE,
    .package = "chromote"
  )

  sibling <- file.path(
    vdir,
    "chrome-headless-shell-linux64",
    "chrome-headless-shell"
  )
  write_fake_file(sibling, "sibling")

  expect_error(install_chrome(), "Could not move the extracted files")

  expect_false(chrome_path_exists(file.path(vdir, "chrome-linux64")))
  expect_no_staging_files(vdir)
  expect_equal(readBin(sibling, "raw", 100), charToRaw("sibling"))
})

test_that("a failed download leaves no staging files or final installation", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) stop("network down"))

  sibling <- file.path(
    vdir,
    "chrome-headless-shell-linux64",
    "chrome-headless-shell"
  )
  write_fake_file(sibling, "sibling")

  expect_error(install_chrome(), "network down")

  expect_false(chrome_path_exists(file.path(vdir, "chrome-linux64")))
  expect_no_staging_files(vdir)
  expect_true(file.exists(sibling))
})

test_that("a failed extraction cleans up the archive and staging directory", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) {
    writeBin(charToRaw("not a zip"), path)
  })

  expect_error(install_chrome())

  expect_false(chrome_path_exists(file.path(vdir, "chrome-linux64")))
  expect_no_staging_files(vdir)
})

test_that("a staged binary that is missing is refused without remove guidance", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) {
    write_fake_zip(path, c("chrome-linux64/other" = "not chrome"))
  })

  err <- expect_error(install_chrome(), class = "error_chrome_invalid_binary")

  expect_equal(err$reason, "missing")
  expect_identical(err$version, test_version)
  expect_match(conditionMessage(err), "Nothing was installed", fixed = TRUE)
  expect_no_match(conditionMessage(err), "chrome_versions_remove", fixed = TRUE)
  expect_false(chrome_path_exists(file.path(vdir, "chrome-linux64")))
  expect_no_staging_files(vdir)
})

test_that("a staged empty binary is refused without remove guidance", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  local_chrome_cache(root, download = function(url, path) {
    write_fake_zip(path, c("chrome-linux64/chrome" = ""))
  })

  err <- expect_error(install_chrome(), class = "error_chrome_invalid_binary")

  expect_equal(err$reason, "empty")
  expect_no_match(conditionMessage(err), "chrome_versions_remove", fixed = TRUE)
  expect_false(chrome_path_exists(file.path(vdir, "chrome-linux64")))
  expect_no_staging_files(vdir)
})

test_that("a valid winner published during download is reused, not overwritten", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  winner <- file.path(vdir, "chrome-linux64", "chrome")
  local_chrome_cache(root, download = function(url, path) {
    write_fake_file(winner, "winner")
    write_fake_zip(path, c("chrome-linux64/chrome" = "download"))
  })

  result <- install_chrome()

  expect_equal(result$path, winner)
  expect_equal(readBin(winner, "raw", 100), charToRaw("winner"))
  expect_no_staging_files(vdir)
})

test_that("an invalid winner published during download is refused, not overwritten", {
  root <- withr::local_tempdir()
  vdir <- file.path(root, "chrome", test_version)
  winner <- file.path(vdir, "chrome-linux64", "chrome")
  local_chrome_cache(root, download = function(url, path) {
    write_fake_file(winner, content = "")
    write_fake_zip(path, c("chrome-linux64/chrome" = "download"))
  })

  err <- expect_error(install_chrome(), class = "error_chrome_invalid_binary")

  expect_equal(err$reason, "empty")
  expect_equal(file.size(winner), 0)
  expect_no_staging_files(vdir)
})
