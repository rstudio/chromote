# chromote 0.1.1.9001

* Fixed #109: An error would occur when a `Chromote` object's `$close()` method was called. (#110)

* Fixed #99: When the `$view()` method was called, recent versions of Chrome would display `"Debugging connection was closed. Reason: WebSocket disconnected"`. (#101)

* Fixed #89, #91: `find_chrome()` now checks more possible binary names for Chrome or Chromium on Linux and Mac. (thanks @brianmsm and @rossellhayes, #117)


# chromote 0.1.1

* Update docs for CRAN (#93)


# chromote 0.1.0

* Initial package release
