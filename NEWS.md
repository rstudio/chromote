# chromote (development version)

* `ChromoteSession` now records the `targetId`. This eliminates one round-trip to the browser when viewing or closing a session. You can now call the `$respawn()` method if a session terminates and you want to reconnect to the same target (#94).

* `ChromoteSession$screenshot()` gains an `options` argument that accepts a list of additional options to be passed to the Chrome Devtools Protocol's [`Page.captureScreenshot` method](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-captureScreenshot). (#129)

* `ChromoteSession$screenshot()` will now infer the image format from the `filename` extension. Alternatively, you can specify the `format` in the list passed to `options`. (#130)

# chromote 0.1.2

* Fixed #109: An error would occur when a `Chromote` object's `$close()` method was called. (#110)

* Fixed #99: When the `$view()` method was called, recent versions of Chrome would display `"Debugging connection was closed. Reason: WebSocket disconnected"`. (#101)

* Fixed #89, #91: `find_chrome()` now checks more possible binary names for Chrome or Chromium on Linux and Mac. (thanks @brianmsm and @rossellhayes, #117)

* Fixed #22: Added a new `chromote.timeout` global option that can be used to set the timeout (in seconds) for establishing connections with the Chrome session. (#120)


# chromote 0.1.1

* Update docs for CRAN (#93)


# chromote 0.1.0

* Initial package release
