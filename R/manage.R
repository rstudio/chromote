#' Use a specific version of Chrome or related binaries
#'
#' This function downloads and sets up a specific version of Chrome, using the
#' [Google Chrome for Testing builds](https://googlechromelabs.github.io/chrome-for-testing)
#' for `chrome`, `chrome-headless-shell` or `chromedriver` for use with
#' chromote.
#'
#' @examplesIf rlang::is_interactive()
#' # Use the latest version of Chrome
#' chrome_use_version()
#'
#' # Use a specific version of chrome-headless-shell
#' chrome_use_version("114.0.5735.90", binary = "chrome-headless-shell")
#'
#' @details This function downloads the specified binary, sets up the necessary
#'   environment, and returns the old CHROMOTE_CHROME environment variable
#'   value. It uses the "known-good-versions" which includes all Google Chrome
#'   for Testing versions from
#'   https://googlechromelabs.github.io/chrome-for-testing
#'
#' @param version A character string specifying the version to use. Default is
#'   `"latest-installed"`, which uses the latest version you have installed via
#'   chromote. To use the current latest version, downloading the binary if
#'   necessary. If you specify a partial version, e.g. `"132"`, chromote will
#'   find the most recent release matching that version. Use
#'   `version = "system"` to revert back to using the system-installed Chrome.
#' @param binary A character string specifying which binary to
#'   use. Must be one of `"chrome"`, `"chrome-headless-shell"`, or
#'   `"chromedriver"`. Default is `"chrome"`.
#' @param platform A character string specifying the platform. If `NULL`
#'   (default), the platform will be automatically detected.
#'
#' @return Sets the `CHROMOTE_CHROME` environment variable and invisibly returns
#'   the old value of the `CHROMOTE_CHROME` environment variable.
#'
#' @export
chrome_use_version <- function(
  version = "latest-installed",
  binary = c("chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL
) {
  old <- Sys.getenv("CHROMOTE_CHROME", unset = "...unset...")
  if (identical(old, "...unset...")) {
    old <- NULL
  }

  if (identical(version, "system")) {
    Sys.unsetenv("CHROMOTE_CHROME")
    cli::cli_inform(
      "chromote will now use {.strong the system-wide installation} of Chrome."
    )
    return(invisible(old))
  }

  binary <- check_binary(binary)

  resolved <- chrome_cache_ensure_binary(
    version = version,
    binary = binary,
    platform = platform
  )

  cli::cli_inform(
    "chromote will now use version {.field {resolved$version}} of {.code {resolved$binary}} for {resolved$platform}."
  )

  Sys.setenv(CHROMOTE_CHROME = resolved$path)
  invisible(old)
}

.chrome_versions <- new.env(parent = emptyenv())

chrome_get_versions <- function(update_cached = TRUE) {
  path_json <- download_json_cached(
    "https://googlechromelabs.github.io/chrome-for-testing/known-good-versions-with-downloads.json",
    update_cached = update_cached
  )

  if (exists(path_json, envir = .chrome_versions)) {
    return(get(path_json, envir = .chrome_versions))
  }

  path_rds <- sub("\\.json$", ".rds", path_json)

  if (file.exists(path_rds)) {
    # Parsing the chrome versions into a tidy data frame takes a little bit, so
    # if we've already done the parsing we store the data as RDS. If the cached
    # object is out-of-date, we re-parse and save the data.
    if (file.info(path_rds)$mtime == file.info(path_json)$mtime) {
      return(readRDS(path_rds))
    }
  }

  res <- jsonlite::fromJSON(path_json, simplifyDataFrame = FALSE)

  res <- res$versions

  res <- lapply(res, function(v) {
    version <- data.frame(version = v$version, revision = v$revision)

    all_versions <- data.frame()

    for (binary_type in names(v$downloads)) {
      binary <- do.call(
        rbind,
        lapply(v$downloads[[binary_type]], as.data.frame)
      )
      binary <- cbind(data.frame(binary = binary_type), binary)
      binary <- cbind(version, binary)
      all_versions <- rbind(all_versions, binary)
    }

    all_versions
  })

  res <- do.call(rbind, res)
  class(res) <- c("tbl_df", "tbl", "data.frame")
  assign(path_json, res, envir = .chrome_versions)

  saveRDS(res, path_rds)
  Sys.setFileTime(path_rds, file.info(path_json)$mtime)
  res
}

#' List installed or available Chrome binary versions
#'
#' By default lists the installed Chrome versions in the [chrome_cache_path()],
#' or list all Chrome versions available via Google's
#' [Chrome for Testing](https://googlechromelabs.github.io/chrome-for-testing)
#' service.
#'
#' @examplesIf rlang::is_interactive()
#' chrome_list_versions()
#'
#' @param which Whether to list `"installed"` local binaries or to list `"all"`
#'   chrome versions available from online sources.
#' @param binary A character string specifying which binary to list. Defaults to
#'   `"all"` to show all binaries, or can be one or more of of `"chrome"`,
#'   `"chrome-headless-shell"`, or `"chromedriver"`.
#' @param platform A character string specifying the platform(s) to list. If
#'   `NULL` (default), the platform will be automatically detected, or if
#'   `"all"`, then binaries for all platforms will be listed.
#'
#' @returns Returns a [data.frame()] of Chrome for Testing versions with
#'   columns: `version`, `revision`, `binary`, `platform`, `url` (where the
#'   binary can be downloaded), and--if `which = "installed"`--the local path to
#'   the binary in the [chrome_cache_path()].
#'
#' @export
chrome_list_versions <- function(
  which = c("installed", "all"),
  binary = c("all", "chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL
) {
  which <- rlang::arg_match(which)
  binary <- check_binary(binary, multiple = TRUE, allow_all = TRUE)
  platform <- check_platform(platform, multiple = TRUE, allow_all = TRUE)

  versions <- chrome_get_versions(update_cached = which == "all")
  versions <- versions[versions$binary %in% binary, ]
  versions <- versions[versions$platform %in% platform, ]

  if (which == "all") {
    return(versions)
  }

  installed <- dir(chrome_cache_path(), include.dirs = TRUE)
  installed <- intersect(installed, unique(versions$version))

  versions <- versions[versions$version %in% installed, ]
  versions$path <- chrome_cache_path(
    versions$version,
    Map(
      chrome_relative_exe,
      binary = versions$binary,
      platform = versions$platform
    )
  )

  versions[file.exists(versions$path), ]
}

#' Chromote cache path helpers
#'
#' @description
#' These functions help interact with \pkg{chromote}'s cache, primarily used to
#' cache Chrome for Testing binaries:
#'
#' * `chromote_cache_path()`: Returns the path to the general \pkg{chromote}
#'   cache.
#' * `chrome_cache_path()`: Returns the path to the cache used for Chrome
#'   binaries (inside the general cache).
#' * `chrome_cache_path_installed()`: Returns a path or paths to specific Chrome
#'   binaries in the cache.
#' * `chromem_cache_remove()`: Remove specific versions and binaries from the
#'   Chrome cache. The `version`, `binary` and `platform` arguments can each
#'   take `"all"` to remove all installed copies of that version, binary or
#'   platform.
#'
#' @param ... Additional path parts.
#' @inheritParams chrome_list_versions
#' @inheritParams chrome_use_version
#'
#' @return A character vector of paths.
#' @name chrome_cache
#' @aliases chromote_cache
NULL

#' @rdname chrome_cache
#' @export
chromote_cache_path <- function(...) {
  file.path(tools::R_user_dir("chromote", which = "cache"), ...)
}

#' @rdname chrome_cache
#' @export
chrome_cache_path <- function(...) {
  chromote_cache_path("chrome", ...)
}

#' @rdname chrome_cache
#' @export
chrome_cache_path_installed <- function(
  version = "latest",
  binary = "chrome",
  platform = NULL
) {
  platform <- check_platform(platform)
  binary <- check_binary(binary)

  versions <- chrome_list_versions(
    which = "installed",
    binary = binary,
    platform = platform
  )

  version_og <- version
  version <- match_version(version, versions$version)

  if (is.null(version)) {
    cli::cli_abort(
      c(
        "Version {.field {version_og}} of {.code {binary}} for {platform} is not installed.",
        "i" = 'Use {.run chromote::chrome_use_version("{version_og}", "{binary}", "{platform}")} to install, or {.run chromote::chrome_list_versions()} to list locally cached versions.'
      )
    )
  }

  versions[versions$version == version, ]$path
}

#' @param ask Whether to ask before removing files.
#'
#' @rdname chrome_cache
#' @export
chrome_cache_remove <- function(version, binary, platform = NULL, ask = TRUE) {
  force(version)
  binary <- check_binary(binary, multiple = TRUE, allow_all = TRUE)
  platform <- check_platform(platform, multiple = TRUE, allow_all = TRUE)

  versions <- chrome_list_versions(
    "installed",
    binary = binary,
    platform = platform
  )

  version <-
    if (identical(version, "all")) {
      versions$version
    } else {
      match_version(version, available_versions = versions$version)
    }

  # versions is already filtered by binary + platform
  to_delete <- versions[versions$version %in% version, ]

  dirs_delete <- chrome_cache_path(
    to_delete$version,
    paste0(to_delete$binary, "-", to_delete$platform)
  )

  if (length(dirs_delete) == 0) {
    cli::cli_inform("No cached binaries to remove.")
    return(invisible())
  }

  if (!identical(ask, FALSE)) {
    cli::cli_inform(
      "Will remove {length(dirs_delete)} cached version{?s} of chrome:"
    )
    cli::cli_bullets(sprintf("{.path %s}", dirs_delete))

    do_delete <- utils::askYesNo("Delete from cache?")
    if (is.na(do_delete) || isFALSE(do_delete)) {
      cli::cli_inform("Canceled.")
      return(invisible(dirs_delete))
    }
  }

  for (path_dir in dirs_delete) {
    path_parent <- dirname(path_dir)
    if (identical(dir(path_parent, full.names = TRUE), path_dir)) {
      # This version contains only the binary being removed...
      unlink(path_parent, recursive = TRUE)
    } else {
      unlink(path_dir, recursive = TRUE)
    }
  }

  invisible(dirs_delete)
}

chrome_cache_ensure_binary <- function(
  version = "latest",
  binary = "chrome",
  platform = NULL,
  prefer_installed = TRUE
) {
  if (identical(version, "latest-installed")) {
    prefer_installed <- TRUE
    version <- "latest"
  } else if (identical(version, "latest")) {
    prefer_installed <- FALSE
  }

  platform <- check_platform(platform)
  binary <- check_binary(binary)

  versions <- if (prefer_installed) {
    chrome_list_versions("installed", binary = binary, platform = platform)
  } else {
    chrome_list_versions("all", binary = binary, platform = platform)
  }

  versions <- versions[
    versions$binary == binary & versions$platform == platform,
  ]

  version_og <- version
  version <- match_version(version, available_versions = versions$version)

  if (is.null(version)) {
    if (prefer_installed) {
      return(
        chrome_cache_ensure_binary(
          version_og,
          binary = binary,
          platform = platform,
          prefer_installed = FALSE
        )
      )
    }
    cli::cli_abort(
      c(
        "Version {.field {version_og}} is not a known {.code {binary}} version.",
        "i" = "Use {.run [chrome_list_versions()](chromote::chrome_list_versions())} to show all available versions."
      )
    )
  }

  url <- versions[versions$version == version, ]$url

  stopifnot(length(url) == 1)

  cache_path <- chrome_cache_path(version)
  binary_path <- file.path(cache_path, chrome_relative_exe(binary, platform))

  resolved <- list(
    path = binary_path,
    version = version,
    binary = binary,
    platform = platform
  )

  # Check if the binary already exists
  if (file.exists(binary_path)) {
    return(resolved)
  }

  old <- options(timeout = max(300, getOption("timeout")))
  on.exit(options(old), add = TRUE)

  cli::cli_progress_step(
    "Downloading {.code {binary}} version {.field {version}} for {platform}"
  )

  dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
  zip_path <- chrome_cache_path("chrome.zip")
  utils::download.file(url, zip_path, mode = "wb")

  zip::unzip(zip_path, exdir = cache_path)

  cli::cli_progress_done()

  if (!ensure_user_exec(binary_path)) {
    cli::cli_abort(
      c(
        "Extracted {.code {binary}} binary does not have execution permissions.",
        "i" = "You may need to manually adjust the permissions of {.path {binary_path}}."
      )
    )
  }

  if (binary == "chrome" && platform %in% c("win32", "win64")) {
    chrome_install_windows_run_setup(binary_path)
  }

  unlink(zip_path)
  resolved
}

chrome_relative_exe <- function(binary, platform) {
  switch(
    binary,
    chrome = chrome_relative_exe_chrome(platform),
    chromedriver = chrome_relative_exe_chromedriver(platform),
    "chrome-headless-shell" = chrome_relative_exe_chrome_headless_shell(
      platform
    )
  )
}

chrome_relative_exe_chrome_headless_shell <- function(platform) {
  # chrome-headless-shell: https://github.com/puppeteer/puppeteer/blob/main/packages/browsers/src/browser-data/chrome-headless-shell.ts
  dir_binary <- paste0("chrome-headless-shell-", platform)

  switch(
    platform,
    "mac-x64" = ,
    "mac-arm64" = ,
    linux64 = file.path(dir_binary, "chrome-headless-shell"),
    win64 = ,
    win32 = file.path(dir_binary, "chrome-headless-shell.exe")
  )
}

chrome_relative_exe_chrome <- function(platform) {
  # chrome: https://github.com/puppeteer/puppeteer/blob/main/packages/browsers/src/browser-data/chrome.ts
  dir_binary <- paste0("chrome-", platform)

  switch(
    platform,
    "mac-x64" = ,
    "mac-arm64" = {
      file.path(
        dir_binary,
        "Google Chrome for Testing.app",
        "Contents",
        "MacOS",
        "Google Chrome for Testing"
      )
    },
    linux64 = file.path(dir_binary, "chrome"),
    win32 = ,
    win64 = file.path(dir_binary, "chrome.exe")
  )
}

chrome_relative_exe_chromedriver <- function(platform) {
  # chromedriver: https://github.com/puppeteer/puppeteer/blob/main/packages/browsers/src/browser-data/chromedriver.ts
  dir_binary <- paste0("chromedriver-", platform)

  switch(
    platform,
    "mac-x64" = ,
    "mac-arm64" = ,
    linux64 = file.path(dir_binary, "chromedriver"),
    win32 = ,
    win64 = file.path(dir_binary, "chromedriver.exe")
  )
}

chrome_platforms <- c("mac-arm64", "mac-x64", "linux64", "win32", "win64")
chrome_binaries <- c("chrome", "chrome-headless-shell", "chromedriver")

check_platform <- function(
  platform = NULL,
  multiple = FALSE,
  allow_all = FALSE
) {
  if (is.null(platform)) {
    return(guess_platform())
  }

  if (allow_all && "all" %in% platform) {
    return(chrome_platforms)
  }

  rlang::arg_match(platform, chrome_platforms, multiple = multiple)
}

guess_platform <- function() {
  os <- Sys.info()["sysname"]
  arch <- Sys.info()["machine"]

  if (os == "Linux" && arch == "x86_64") {
    return("linux64")
  } else if (os == "Darwin") {
    if (arch == "arm64") {
      return("mac-arm64")
    } else if (arch == "x86_64") {
      return("mac-x64")
    }
  } else if (os == "Windows") {
    if (arch == "x86_64") {
      return("win64")
    } else if (arch == "x86") {
      return("win32")
    }
  }

  cli::cli_abort(
    "Chrome is not available for {.val {os}} (OS) and {.val {arch}} (arch)."
  )
}

check_binary <- function(binary, multiple = FALSE, allow_all = FALSE) {
  if (allow_all && "all" %in% binary) {
    return(chrome_binaries)
  }

  rlang::arg_match(binary, chrome_binaries, multiple = multiple)
}

match_version <- function(version, available_versions = NULL) {
  stopifnot(length(version) == 1)

  if (!is.character(version)) {
    if (as.integer(version) != version) {
      rlang::abort(
        "`version` must be an character version number or an integer."
      )
    }
    version <- as.character(version)
  }

  if (is.null(available_versions)) {
    available_versions <- unique(chrome_get_versions()$version)
  }

  if (identical(version, "latest")) {
    return(max(available_versions))
  }

  available_versions <- numeric_version(unique(available_versions))

  version_parts <- unclass(numeric_version(version))[[1]]

  max_version <- rep(99999, 4)
  max_version[seq_along(version_parts)] <- version_parts
  max_version <- numeric_version(paste(max_version, collapse = "."))

  min_version <- rep(0, 4)
  min_version[seq_along(version_parts)] <- version_parts
  min_version <- numeric_version(paste(min_version, collapse = "."))

  available_versions <- available_versions[
    available_versions <= max_version &
      available_versions >= min_version
  ]

  if (length(available_versions) == 0) {
    return(NULL)
  }

  max(available_versions)
}

curl_fetch_headers <- function(url) {
  h <- curl::new_handle()
  curl::handle_setopt(h, nobody = TRUE)
  req <- curl::curl_fetch_memory(url, handle = h)
  req_parse_headers(req)
}

req_parse_headers <- function(req) {
  headers <- rawToChar(req$headers)
  parsed_headers <- strsplit(headers, "\r\n")[[1]]
  parsed_headers <- parsed_headers[parsed_headers != ""]
  parsed_headers <- strsplit(parsed_headers, ": ")
  parsed_headers <- rlang::set_names(
    lapply(parsed_headers, `[`, 2),
    sapply(parsed_headers, `[`, 1)
  )

  parsed_headers
}

download_json_cached <- function(url, update_cached = TRUE) {
  path_cache <- chromote_cache_path()
  dir.create(path_cache, showWarnings = FALSE, recursive = TRUE)

  path_local <- file.path(path_cache, basename(url))

  # Check if local file exists and get its modified time
  if (file.exists(path_local)) {
    if (!update_cached) {
      return(path_local)
    }

    local_mtime <- file.info(path_local)$mtime

    is_local_stale <- tryCatch(
      {
        # Fetch headers from the server
        headers <- curl_fetch_headers(url)
        server_last_modified <- as.POSIXct(
          headers[["last-modified"]],
          format = "%a, %d %b %Y %H:%M:%S GMT",
          tz = "GMT"
        )

        length(server_last_modified) == 1 && local_mtime < server_last_modified
      },
      error = function(err) {
        rlang::inform(
          "Could not reach Chrome for Testing to update available versions.",
          parent = err
        )
        FALSE
      }
    )

    # Compare local file time with server's last-modified
    if (is_local_stale) {
      # message("Source URL not modified, using cached version")
      return(path_local)
    }
  }

  req <- curl::curl_fetch_memory(url)

  if (req$status_code == 200) {
    # message("Source URL was updated, downloading new content")
    json_content <- rawToChar(req$content)
    writeLines(json_content, path_local)

    # Set the local file's modified time to the last-modified
    headers <- req_parse_headers(req)
    last_modified <- as.POSIXct(
      headers[["last-modified"]],
      format = "%a, %d %b %Y %H:%M:%S GMT",
      tz = "GMT"
    )
    Sys.setFileTime(path_local, last_modified)
  } else {
    stop("Could not download ", url, ": Status code ", req$status_code)
  }

  path_local
}

ensure_user_exec <- function(path) {
  current_mode <- file.info(path)$mode
  user_perm <- as.numeric(as.character(current_mode))

  # If user permissions is even, the file is not executable
  !((user_perm %/% 100) %% 2 == 0)
}

chrome_install_windows_run_setup <- function(path) {
  path_setup <- file.path(dirname(path), "setup.exe")
  if (!file.exists(path_setup)) {
    return()
  }

  processx::run(
    path_setup,
    args = sprintf("--configure-browser-in-directory=%s", dirname(path))
  )
}
