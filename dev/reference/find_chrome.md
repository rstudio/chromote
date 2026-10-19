# Find path to Chrome or Chromium browser

chromote requires a Chrome- or Chromium-based browser with support for
the Chrome DevTools Protocol. There are many such browser variants,
including [Google Chrome](https://www.google.com/chrome/),
[Chromium](https://www.chromium.org/chromium-projects/), [Microsoft
Edge](https://www.microsoft.com/en-us/edge) and others.

If you want chromote to use a specific browser, set the
`CHROMOTE_CHROME` environment variable to the full path to the browser's
executable. Note that when `CHROMOTE_CHROME` is set, chromote will use
the value without any additional checks. On Mac, for example, one could
use Microsoft Edge by setting `CHROMOTE_CHROME` with the following:

    Sys.setenv(
      CHROMOTE_CHROME = "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge"
    )

When `CHROMOTE_CHROME` is not set, `find_chrome()` will perform a
limited search to find a reasonable executable. On Windows,
`find_chrome()` consults the registry to find `chrome.exe`. On Mac, it
looks for `Google Chrome` in the `/Applications` folder (or tries the
same checks as on Linux). On Linux, it searches for several common
executable names.

## Usage

``` r
find_chrome()
```

## Value

A character vector with the value of `CHROMOTE_CHROME`, or a path to the
discovered Chrome executable. If no path to is found, `find_chrome()`
returns `NULL`.

## Examples

``` r
find_chrome()
#> [1] "/usr/bin/google-chrome"
```
