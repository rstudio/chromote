# List installed or available Chrome binary versions

**\[e\]**

By default lists the installed Chrome versions in the
[`chrome_versions_path_cache()`](https://rstudio.github.io/chromote/dev/reference/chrome_versions.md),
or list all Chrome versions available via Google's [Chrome for
Testing](https://googlechromelabs.github.io/chrome-for-testing/)
service.

Managed Chrome installations is an experimental feature introduced in
chromote v0.5.0 and was inspired by similar features in
[playwright](https://playwright.dev/).

## Usage

``` r
chrome_versions_list(
  which = c("installed", "all"),
  binary = c("all", "chrome", "chrome-headless-shell", "chromedriver"),
  platform = NULL
)
```

## Arguments

- which:

  Whether to list `"installed"` local binaries or to list `"all"` chrome
  versions available from online sources.

- binary:

  A character string specifying which binary to list. Defaults to
  `"all"` to show all binaries, or can be one or more of of `"chrome"`,
  `"chrome-headless-shell"`, or `"chromedriver"`.

- platform:

  A character string specifying the platform(s) to list. If `NULL`
  (default), the platform will be automatically detected, or if `"all"`,
  then binaries for all platforms will be listed.

## Value

Returns a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) of
Chrome for Testing versions with columns: `version`, `revision`,
`binary`, `platform`, `url` (where the binary can be downloaded), and–if
`which = "installed"`–the local path to the binary in the
[`chrome_versions_path_cache()`](https://rstudio.github.io/chromote/dev/reference/chrome_versions.md).

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
chrome_versions_list()
}
```
