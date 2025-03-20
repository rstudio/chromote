# chromote 0.5.0

## New features

* chromote now includes experimental features to download versioned binaries of Chrome and `chrome-headless-shell` for Mac (x64 or arm64), Windows (32- or 64-bit) or Linux (x86-64) from the [Chrome for Testing](https://googlechromelabs.github.io/chrome-for-testing/) service. (#198)
  * Use `with_chrome_version()` or `local_chrome_version()` to temporarily switch to a specific version of Chrome. The appropriate binary will be downloaded automatically if not yet available locally. 
  * Use `chrome_versions_list()` to list installed or available versions of Chrome. 
  * Or use `chrome_versions_add()` and `chrome_versions_remove()` to manually add or remove a specific version of Chrome from chromote's cache.

* `ChromoteSession` gains two new helper methods: `$set_viewport_size()` and `$get_viewport_size()`. These methods allow you to change the viewport size – effectively the virtual window size for a page – or to get the current viewport size. If you previously relied on `$Emulation$setVisibleSize()` (now a deprecated method in the Chrome DevTools Protocol), `$set_viewport_size()` is a good replacement as it uses [Emulation.setDeviceMetricsOverride](https://chromedevtools.github.io/devtools-protocol/tot/Emulation/#method-setDeviceMetricsOverride) instead. (#206)

## Improvements

* `ChromoteSession$new()` gains a `mobile` argument that can be used to set the device emulation in that session to emulate a mobile browser. The default is `mobile = FALSE`, which matches previous behavior. (#205)

* `Chromote` and `ChromoteSesssion` gain an `$auto_events_enable_args()` method that sets that arguments used by chromote's auto-events feature when calling the `enable` command for a domain, e.g. `Fetch.enable`. (#208)

* The `$view()` method of a `ChromoteSession` will now detect when `chrome-headless-shell` is being used and will use the system browser (via `utils::browseURL()`) rather than the Chrome instance attached to chromote. (#214)

* chromote now has a hex sticker! Thank you to @davidrsch for the inspiration. (#216)

## Bug fixes

* `ChromoteSession$new()` now sets `width` and `height` using [Emulation.setDeviceMetricsOverride](https://chromedevtools.github.io/devtools-protocol/tot/Emulation/#method-setDeviceMetricsOverride), which works for all Chrome binaries and versions. This fixes an issue with `width` and `height` being ignored for Chrome versions 128-133. (#205)

* Fixed a bug in `chromote_info()` on Windows with Powershell when no version info is returned. (#207)

* `Chromote` and `ChromoteSession` once again correctly handles connections to remote Chrome browsers via `ChromeRemote`. Calling `$close()` on a `Chromote` object connected to a remote browser no longer attempts to close the browser, and will now simply close the websocket connection to the browser. For local process, the `Chromote$close()` gains a `wait` argument that sets the number of seconds to wait for Chrome to gracefully shut down before chromote closes the process. (#212)

# chromote 0.4.0

* Chrome v132 and later no longer support [old headless mode](https://developer.chrome.com/blog/removing-headless-old-from-chrome). As such, `chromote` no longer defaults to using `--headless=old` and now uses `--headless` when running Chrome. You can still use the `chromote.headless` option or `CHROMOTE_HEADLESS` environment variable to configure the `--headless` flag if you're using an older version of Chrome. (#187)

* Added `chromote_info()`, a new utility function to print out key information about chromote and Chrome. Useful when debugging chromote or reporting an issue. (#190)

* chromote now uses a consistent prefix for logs, e.g `{tempdir}/chrome-{id}-stdout.log` and `{tempdir}/chrome-{id}-stderr.log`. chromote also now uses `--crash-dumps-dir` to set a session-specific temp directory. (#194)

# chromote 0.3.1

* Fixed a typo that caused `launch_chrome()` to throw an error. (#175)

# chromote 0.3.0

* The headless mode used by Chrome can now be selected with the `chromote.headless` option or `CHROMOTE_HEADLESS` environment variable. 

  In Chrome v128, a [new headless mode](https://developer.chrome.com/docs/chromium/new-headless) became the default. The new mode uses the same browser engine as the regular Chrome browser, whereas the old headless mode is built on a separate architecture. The old headless mode may be faster to launch and is still well-suited to many of the tasks for which chromote is used.

  For now, to avoid disruption, chromote defaults to using the old headless mode. In the future, chromote will follow Chrome and default to `"new"` headless mode. (And at some point, Chrome intends to remove the old headless mode which is now offered as [a separate binary](https://developer.chrome.com/blog/chrome-headless-shell).) To test the new headless mode, use `options(chromote.headless = "new")` or `CHROMOTE_HEADLESS="new"` (in `.Renviron` or via `Sys.setenv()`). (#172)

# chromote 0.2.0

## Breaking changes

* Breaking change: `Chromote$is_active()` method now reports if there is an active connection to the underlying chrome instance, rather than whether or not that instance is alive (#94).

## Improvements and bug fixes

* `Chromote` and `ChromoteSession` gain print methods to give you a snapshot of the most important values (#140).

* `Chromote` gains a new `is_alive()` method equivalent to the old `is_active()` method; i.e. it reports on if there is an active chrome process running in the background (#136).

* `ChromoteSession` now records the `targetId`. This eliminates one round-trip to the browser when viewing or closing a session. You can now call the `$respawn()` method if a session terminates and you want to reconnect to the same target (#94).

* `ChromoteSession$screenshot()` gains an `options` argument that accepts a list of additional options to be passed to the Chrome Devtools Protocol's [`Page.captureScreenshot` method](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-captureScreenshot) (#129).

* `ChromoteSession$screenshot()` will now infer the image format from the `filename` extension. Alternatively, you can specify the `format` in the list passed to `options` (#130).

* `--disable-gpu` is no longer included in the default Chrome arguments, except on windows where empirically it appears to be necessary (otherwise GHA check runs never terminate) (#142).

# chromote 0.1.2

* Fixed #109: An error would occur when a `Chromote` object's `$close()` method was called. (#110)

* Fixed #99: When the `$view()` method was called, recent versions of Chrome would display `"Debugging connection was closed. Reason: WebSocket disconnected"`. (#101)

* Fixed #89, #91: `find_chrome()` now checks more possible binary names for Chrome or Chromium on Linux and Mac. (thanks @brianmsm and @rossellhayes, #117)

* Fixed #22: Added a new `chromote.timeout` global option that can be used to set the timeout (in seconds) for establishing connections with the Chrome session. (#120)


# chromote 0.1.1

* Update docs for CRAN (#93)


# chromote 0.1.0

* Initial package release
