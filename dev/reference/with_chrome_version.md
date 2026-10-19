# Use a specific version of Chrome or related binaries

**\[e\]**

This function downloads and sets up a specific version of Chrome, using
the [Google Chrome for Testing
builds](https://googlechromelabs.github.io/chrome-for-testing/) for
`chrome`, `chrome-headless-shell` or `chromedriver` for use with
chromote.

Managed Chrome installations is an experimental feature introduced in
chromote v0.5.0 and was inspired by similar features in
[playwright](https://playwright.dev/).

## Usage

``` r
with_chrome_version(
  version = "latest-stable",
  code,
  ...,
  binary = c("chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL,
  quiet = TRUE
)

local_chrome_version(
  version = "latest-stable",
  binary = c("chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL,
  ...,
  quiet = FALSE,
  .local_envir = parent.frame()
)

local_chromote_chrome(path, ..., .local_envir = parent.frame())

with_chromote_chrome(path, code, ...)
```

## Arguments

- version:

  A character string specifying the version to use. The default value is
  `"latest-stable"` to follow the latest stable release of Chrome. For
  robust results, and to avoid frequently downloading new versions of
  Chrome, use a fully qualified version number, e.g. `"133.0.6943.141"`.

  If you specify a partial version, e.g. `"133"`, chromote will find the
  most recent release matching that version, preferring to use the
  latest *installed* release that matches the partially-specified
  version. chromote also supports a few special version names:

  - `"latest-installed"`: The latest version currently installed locally
    in chromote's cache. If you don't have any installed versions of the
    binary, chromote uses `"latest"`.

  - `"latest"`: The most recent Chrome for Testing release, which may be
    a beta or canary release.

  - `"latest-stable"`, `"latest-beta"`, `"latest-extended"`,
    `"latest-canary"` or `"latest-dev"`: Installs the latest release
    from one of Chrome's version channels, queried from the
    [VersionHistory
    API](https://developer.chrome.com/docs/web-platform/versionhistory/reference#platform-identifiers).
    `"latest-stable"` is the default value of `with_chrome_version()`
    and `local_chrome_version()`.

  - `"system"`: Use the system-wide installation of Chrome.

  Chromote also supports

- code:

  `[any]`  
  Code to execute in the temporary environment

- ...:

  Ignored, used to require named arguments and for future feature
  expansion.

- binary:

  A character string specifying which binary to use. Must be one of
  `"chrome"`, `"chrome-headless-shell"`, or `"chromedriver"`. Default is
  `"chrome"`.

- platform:

  A character string specifying the platform. If `NULL` (default), the
  platform will be automatically detected.

- quiet:

  Whether to print a message indicating which version and binary of
  Chrome is being used. By default, this message is suppressed for
  `with_chrome_version()` and enabled for `local_chrome_version()`.

- .local_envir:

  `[environment]`  
  The environment to use for scoping.

- path:

  A direct path to the Chrome (or Chrome-based) binary. See
  [`find_chrome()`](https://rstudio.github.io/chromote/dev/reference/find_chrome.md)
  for details or
  [`chrome_versions_path()`](https://rstudio.github.io/chromote/dev/reference/chrome_versions.md)
  for paths from the chromote-managed cache.

## Value

Temporarily sets the `CHROMOTE_CHROME` environment variable and returns
the result of the `code` argument.

## Details

This function downloads the specified binary, if not already available
and configures
[`find_chrome()`](https://rstudio.github.io/chromote/dev/reference/find_chrome.md)
to use the specified binary while evaluating `code` or within the local
scope. It uses the "known-good-versions" list from the Google Chrome for
Testing versions at
<https://googlechromelabs.github.io/chrome-for-testing/>.

## Functions

- `with_chrome_version()`: Temporarily use a specific version of Chrome
  during the evaluation of `code`.

- `local_chrome_version()`: Use a specific version of Chrome within the
  current scope.

- `local_chromote_chrome()`: Use a specific Chrome, by path, within the
  current scope.

- `with_chromote_chrome()`: Temporarily use a specific Chrome version,
  by path, for the evaluation of `code`.

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
# Use the latest version of Chrome
local_chrome_version()

# Use a specific version of chrome-headless-shell
local_chrome_version("114.0.5735.90", binary = "chrome-headless-shell")
}
```
