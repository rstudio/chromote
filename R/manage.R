#' Use a specific version of Chrome or related binaries
#'
#' @description
#' `r lifecycle_badge("experimental")`
#'
#' This function downloads and sets up a specific version of Chrome, using the
#' [Google Chrome for Testing builds](https://googlechromelabs.github.io/chrome-for-testing/)
#' for `chrome`, `chrome-headless-shell` or `chromedriver` for use with
#' chromote.
#'
#' Managed Chrome installations is an experimental feature introduced in
#' chromote v0.5.0 and was inspired by similar features in
#' [playwright](https://playwright.dev/).
#'
#' @examplesIf rlang::is_interactive()
#' # Use the latest version of Chrome
#' local_chrome_version()
#'
#' # Use a specific version of chrome-headless-shell
#' local_chrome_version("114.0.5735.90", binary = "chrome-headless-shell")
#'
#' @details This function downloads the specified binary, if not already
#'   available and configures [find_chrome()] to use the specified binary while
#'   evaluating `code` or within the local scope. It uses the
#'   "known-good-versions" list from the Google Chrome for Testing versions at
#'   <https://googlechromelabs.github.io/chrome-for-testing/>.
#'
#' @param version A character string specifying the version to use. The default
#'   value is `"latest-stable"` to follow the latest stable release of Chrome.
#'   For robust results, and to avoid frequently downloading new versions of
#'   Chrome, use a fully qualified version number, e.g. `"133.0.6943.141"`.
#'
#'   If you specify a partial version, e.g. `"133"`, chromote will find the most
#'   recent release matching that version, preferring to use the latest
#'   *installed* release that matches the partially-specified version. chromote
#'   also supports a few special version names:
#'
#'   * `"latest-installed"`: The latest version currently installed locally in
#'     chromote's cache. If you don't have any installed versions of the binary,
#'     chromote uses `"latest"`.
#'   * `"latest"`: The most recent Chrome for Testing release, which may be a
#'     beta or canary release.
#'   * `"latest-stable"`, `"latest-beta"`, `"latest-extended"`,
#'     `"latest-canary"` or `"latest-dev"`: Installs the latest release from one
#'     of Chrome's version channels, queried from the
#'     [VersionHistory API](https://developer.chrome.com/docs/web-platform/versionhistory/reference#platform-identifiers).
#'     `"latest-stable"` is the default value of `with_chrome_version()` and
#'     `local_chrome_version()`.
#'   * `"system"`: Use the system-wide installation of Chrome.
#'
#'   Chromote also supports
#' @param binary A character string specifying which binary to
#'   use. Must be one of `"chrome"`, `"chrome-headless-shell"`, or
#'   `"chromedriver"`. Default is `"chrome"`.
#' @param platform A character string specifying the platform. If `NULL`
#'   (default), the platform will be automatically detected.
#' @param quiet Whether to print a message indicating which version and binary
#'   of Chrome is being used. By default, this message is suppressed for
#'   [with_chrome_version()] and enabled for [local_chrome_version()].
#' @inheritParams withr::local_envvar
#' @param ... Ignored, used to require named arguments and for future feature
#'   expansion.
#'
#' @return Temporarily sets the `CHROMOTE_CHROME` environment variable and
#'   returns the result of the `code` argument.
#'
#' @describeIn with_chrome_version Temporarily use a specific version of Chrome
#'   during the evaluation of `code`.
#' @export
with_chrome_version <- function(
  version = "latest-stable",
  code,
  ...,
  binary = c("chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL,
  quiet = TRUE
) {
  rlang::check_dots_empty()

  local_chrome_version(
    version = version,
    binary = binary,
    platform = platform,
    quiet = quiet
  )
  force(code)
}

#' @describeIn with_chrome_version Use a specific version of Chrome within the
#'   current scope.
#' @export
local_chrome_version <- function(
  version = "latest-stable",
  binary = c("chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL,
  ...,
  quiet = FALSE,
  .local_envir = parent.frame()
) {
  rlang::check_dots_empty()

  if (identical(version, "system")) {
    if (!quiet)
      cli::cli_inform(
        "chromote will now use {.strong the system-wide installation} of Chrome."
      )
    return(local_chromote_chrome("", .local_envir = .local_envir))
  }

  binary <- check_binary(binary)

  resolved <- chrome_versions_ensure(
    version = version,
    binary = binary,
    platform = platform
  )

  if (!quiet && !identical(version, resolved$version)) {
    cli::cli_inform(
      "chromote will now use version {.field {resolved$version}} of {.code {resolved$binary}} for {resolved$platform}."
    )
  }

  local_chromote_chrome(resolved$path, .local_envir = .local_envir)
}

#' @param path A direct path to the Chrome (or Chrome-based) binary. See
#'   [find_chrome()] for details or [chrome_versions_path()] for paths
#'   from the chromote-managed cache.
#' @describeIn with_chrome_version Use a specific Chrome, by path, within the
#'   current scope.
#' @export
local_chromote_chrome <- function(path, ..., .local_envir = parent.frame()) {
  rlang::check_dots_empty()

  old_default_chromote_object <-
    if (has_default_chromote_object()) default_chromote_object() else NULL

  withr::defer(
    {
      if (has_default_chromote_object()) {
        current <- default_chromote_object()
        current$close()
      }

      if (is.null(old_default_chromote_object)) {
        globals$default_chromote <- NULL
      } else if (old_default_chromote_object$is_alive()) {
        set_default_chromote_object(old_default_chromote_object)
      } else {
        globals$default_chromote <- NULL
      }
    },
    envir = .local_envir
  )

  # We always create a *new* Chromote process within `local_chromote_chrome()`
  # that we completely clean up when the exit handlers run. We do this by
  # unsetting the current chromote default so that next ChromoteSession uses a
  # new Chromote obj, side-stepping `set_default_chromote_object()` because that
  # requires a chromote obj that we don't want to create yet.
  globals$default_chromote <- NULL

  withr::local_envvar(
    list(CHROMOTE_CHROME = path),
    .local_envir = .local_envir,
    action = "replace"
  )
}

#' @describeIn with_chrome_version Temporarily use a specific Chrome version, by
#'   path, for the evaluation of `code`.
#' @export
with_chromote_chrome <- function(path, code, ...) {
  rlang::check_dots_empty()
  local_chromote_chrome(path)
  force(code)
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
#' @description
#' `r lifecycle_badge("experimental")`
#'
#' By default lists the installed Chrome versions in the [chrome_versions_path_cache()],
#' or list all Chrome versions available via Google's
#' [Chrome for Testing](https://googlechromelabs.github.io/chrome-for-testing/)
#' service.
#'
#' Managed Chrome installations is an experimental feature introduced in
#' chromote v0.5.0 and was inspired by similar features in
#' [playwright](https://playwright.dev/).
#'
#' @examplesIf rlang::is_interactive()
#' chrome_versions_list()
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
#'   the binary in the [chrome_versions_path_cache()].
#'
#' @export
chrome_versions_list <- function(
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
  versions <- versions[
    order(numeric_version(versions$version), decreasing = TRUE),
  ]

  if (which == "all") {
    return(versions)
  }

  installed <- dir(chrome_versions_path_cache(), include.dirs = TRUE)
  installed <- intersect(installed, unique(versions$version))

  versions <- versions[versions$version %in% installed, ]
  versions$path <- chrome_versions_path_cache(
    versions$version,
    Map(
      chrome_relative_exe,
      binary = versions$binary,
      platform = versions$platform
    )
  )

  versions[file.exists(versions$path), ]
}

#' Chrome versions cache helpers
#'
#' @description
#' `r lifecycle_badge("experimental")`
#'
#' These functions help interact with the cache used by \pkg{chromote}'s for
#' storing versioned Chrome for Testing binaries:
#'
#' * `chrome_versions_path()`: Returns a path or paths to specific Chrome
#'   binaries in the cache.
#' * `chrome_versions_add()`: Add a specific version to the Chrome versions
#'   cache.
#' * `chrome_versions_remove()`: Remove specific versions and binaries from the
#'   Chrome cache. The `version`, `binary` and `platform` arguments can each
#'   take `"all"` to remove all installed copies of that version, binary or
#'   platform.
#' * `chrome_versions_path_cache()`: Returns the path to the cache directory
#'   used for Chrome binaries.
#'
#' Managed Chrome installations is an experimental feature introduced in
#' chromote v0.5.0 and was inspired by similar features in
#' [playwright](https://playwright.dev/).
#'
#' @seealso [chrome_versions_list()]
#'
#' @param ... Additional path parts.
#' @param version A character string specifying the version to list, add or
#'   remove.
#' @inheritParams chrome_versions_list
#' @inheritParams with_chrome_version
#'
#' @return A character vector of Chrome binary paths.
#' @name chrome_versions
NULL

#' @rdname chrome_versions
#' @export
chrome_versions_path_cache <- function(...) {
  chromote_cache_path("chrome", ...)
}

# Not exported
chromote_cache_path <- function(...) {
  cache_base <- normalizePath(
    tools::R_user_dir("chromote", which = "cache"),
    mustWork = FALSE,
    winslash = "/"
  )
  file.path(cache_base, ...)
}

#' @rdname chrome_versions
#' @export
chrome_versions_path <- function(
  version = "latest",
  binary = "chrome",
  platform = NULL
) {
  platform <- check_platform(platform)
  binary <- check_binary(binary)

  versions <- chrome_versions_list(
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
        "i" = 'Use {.run chromote::chrome_versions_add("{version_og}", "{binary}", "{platform}")} to install, or {.run chromote::chrome_versions_list()} to list locally cached versions.'
      )
    )
  }

  versions[versions$version == version, ]$path
}

#' @rdname chrome_versions
#' @export
chrome_versions_add <- function(version, binary, platform = NULL) {
  res <- chrome_versions_ensure(version, binary, platform)

  res[["path"]]
}

#' @param ask Whether to ask before removing files.
#'
#' @rdname chrome_versions
#' @export
chrome_versions_remove <- function(
  version,
  binary,
  platform = NULL,
  ask = TRUE
) {
  force(version)
  binary <- check_binary(binary, multiple = TRUE, allow_all = TRUE)
  platform <- check_platform(platform, multiple = TRUE, allow_all = TRUE)

  if (grepl("latest|system", version)) {
    cli::cli_abort(c(
      "{.fn chrome_versions_remove} does not support deleting versions by keyword.",
      "i" = "Please use {.run chromote::chrome_versions_list()} to list installed versions."
    ))
  }

  versions <- chrome_versions_list(
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

  dirs_delete <- chrome_versions_path_cache(
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

    cli::cli_inform("Delete from cache?")
    do_delete <- utils::menu(gettext(c("Yes", "No", "Cancel")))
    if (do_delete != 1L) {
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

chrome_versions_ensure <- function(
  version = "latest",
  binary = "chrome",
  platform = NULL,
  prefer_installed = TRUE
) {
  platform <- check_platform(platform)
  binary <- check_binary(binary)
  if (length(version) != 1) {
    cli::cli_abort(
      "`version` must be a single string or integer value, not {.val {version}}."
    )
  }

  requested_latest_installed <- identical(version, "latest-installed")

  if (requested_latest_installed) {
    prefer_installed <- TRUE
    version <- "latest"
  } else if (identical(version, "latest")) {
    prefer_installed <- FALSE
  } else if (grepl("^latest-", version)) {
    version <- chrome_resolve_latest_channel(version, platform)
    prefer_installed <- TRUE
  }

  versions <- if (prefer_installed) {
    chrome_versions_list("installed", binary = binary, platform = platform)
  } else {
    chrome_versions_list("all", binary = binary, platform = platform)
  }

  versions <- versions[
    versions$binary == binary & versions$platform == platform,
  ]

  version_og <- version
  version <- match_version(version, available_versions = versions$version)

  if (is.null(version)) {
    if (prefer_installed) {
      return(
        chrome_versions_ensure(
          if (requested_latest_installed) "latest-stable" else version_og,
          binary = binary,
          platform = platform,
          prefer_installed = FALSE
        )
      )
    }
    cli::cli_abort(
      c(
        "Version {.field {version_og}} is not a known {.code {binary}} version.",
        "i" = "Use {.run [chrome_versions_list()](chromote::chrome_versions_list())} to show all available versions."
      )
    )
  }

  url <- versions[versions$version == version, ]$url

  stopifnot(length(url) == 1)

  cache_path <- chrome_versions_path_cache(version)
  binary_path <- file.path(cache_path, chrome_relative_exe(binary, platform))

  resolved <- list(
    path = binary_path,
    version = version,
    binary = binary,
    platform = platform
  )

  if (file.exists(binary_path)) {
    return(resolved)
  }

  old <- options(timeout = max(300, getOption("timeout")))
  on.exit(options(old), add = TRUE)

  cli::cli_progress_step(
    "Downloading {.code {binary}} version {.field {version}} for {platform}"
  )

  dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
  zip_path <- chrome_versions_path_cache("chrome.zip")
  withr::with_options(list(timeout = max(20 * 60, getOption("timeout"))), {
    utils::download.file(url, zip_path, mode = "wb")
  })

  zip::unzip(zip_path, exdir = cache_path)

  cli::cli_progress_done()

  if (!file.exists(binary_path)) {
    cli::cli_abort(
      c(
        "The Chrome binary was not found at the expected path.",
        "x" = "Expected {.path {binary_path}}",
        "i" = "The downloaded zip was not deleted: {.path {zip_path}}",
        "i" = "If the problem persists, please report this issue to {.href [rstudio/chromote](https://github.com/rstudio/chromote/issues/new)}."
      )
    )
  }

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
  check_binary(binary)

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
  check_platform(platform)
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
  check_platform(platform)
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
  check_platform(platform)
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

  is_arch_x86_64 <- grepl("^x86[_-]64$", arch)

  if (os == "Linux" && is_arch_x86_64) {
    return("linux64")
  } else if (os == "Darwin") {
    if (arch == "arm64") {
      return("mac-arm64")
    } else if (is_arch_x86_64) {
      return("mac-x64")
    }
  } else if (os == "Windows") {
    if (is_arch_x86_64) {
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

  if (length(available_versions) == 0) {
    return(NULL)
  }

  if (is.null(available_versions)) {
    available_versions <- unique(chrome_get_versions()$version)
  }

  if (identical(version, "latest")) {
    return(max(numeric_version(available_versions)))
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

req_headers_last_modified <- function(headers) {
  names(headers) <- tolower(names(headers))

  if (!"last-modified" %in% names(headers)) {
    return(NULL)
  }

  withr::with_locale(new = c("LC_TIME" = "C"), {
    last_modified <- as.POSIXct(
      headers[["last-modified"]],
      format = "%a, %d %b %Y %H:%M:%S GMT",
      tz = "GMT"
    )
    last_modified
  })
}

chrome_resolve_latest_channel <- function(
  channel,
  platform = guess_platform()
) {
  channel <- sub("latest-", "", channel)

  path_json <- download_json_cached(
    chrome_version_history_url(channel, platform),
    filename = sprintf("chrome-version-history_%s_%s.json", platform, channel)
  )

  res <- jsonlite::fromJSON(path_json)$versions

  testing_versions <- chrome_versions_list("all", "chrome", platform)

  available_versions <- intersect(res$version, testing_versions$version)

  as.character(match_version("latest", available_versions))
}

chrome_version_history_url <- function(
  channel = c("stable", "beta", "extended", "dev", "canary"),
  platform = guess_platform()
) {
  channel <- rlang::arg_match(channel)
  platform <- check_platform(platform)

  platform <- switch(
    platform,
    win32 = "win",
    win64 = "win64",
    "mac-x64" = "mac",
    "mac-arm64" = "mac_arm64",
    "linux64" = "linux"
  )

  sprintf(
    "https://versionhistory.googleapis.com/v1/chrome/platforms/%s/channels/%s/versions",
    platform,
    channel
  )
}

download_json_cached <- function(url, update_cached = TRUE, filename = NULL) {
  path_cache <- chromote_cache_path()
  dir.create(path_cache, showWarnings = FALSE, recursive = TRUE)

  path_local <- file.path(path_cache, filename %||% basename(url))

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
        server_last_modified <- req_headers_last_modified(headers)

        if (!is.null(server_last_modified)) {
          length(server_last_modified) == 1 &&
            local_mtime < server_last_modified
        } else {
          # otherwise cache for 8 hours
          (local_mtime + 60 * 60 * 8) < Sys.time()
        }
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
    if (!is_local_stale) {
      # message("Source URL not modified, using cached version")
      return(path_local)
    }
  }

  req <- curl::curl_fetch_memory(url)

  if (!req$status_code == 200) {
    cli::cli_abort(
      "Could not download {.url {url}}. Status code: {.field {req$status_code}}",
      status = req$status_code,
      request = req
    )
  }

  # message("Source URL was updated, downloading new content")
  json_content <- rawToChar(req$content)
  writeLines(json_content, path_local)

  # Set the local file's modified time to the last-modified
  last_modified <- req_headers_last_modified(req_parse_headers(req))
  if (!is.null(last_modified)) {
    Sys.setFileTime(path_local, last_modified)
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

  tryCatch(
    {
      processx::run(
        path_setup,
        args = sprintf("--configure-browser-in-directory=%s", dirname(path))
      )
    },
    error = function(err) {
      cli::cli_warn(
        "Running Chrome's {.field setup.exe} failed, which may not mean anything or it may mean that you need to manually resolve permissions errors.",
        parent = err
      )
      return()
    }
  )
}
