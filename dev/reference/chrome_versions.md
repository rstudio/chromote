# Chrome versions cache helpers

**\[e\]**

These functions help interact with the cache used by chromote's for
storing versioned Chrome for Testing binaries:

- `chrome_versions_path()`: Returns a path or paths to specific Chrome
  binaries in the cache.

- `chrome_versions_add()`: Add a specific version to the Chrome versions
  cache.

- `chrome_versions_remove()`: Remove specific versions and binaries from
  the Chrome cache. The `version`, `binary` and `platform` arguments can
  each take `"all"` to remove all installed copies of that version,
  binary or platform.

- `chrome_versions_path_cache()`: Returns the path to the cache
  directory used for Chrome binaries.

Managed Chrome installations is an experimental feature introduced in
chromote v0.5.0 and was inspired by similar features in
[playwright](https://playwright.dev/).

## Usage

``` r
chrome_versions_path_cache(...)

chrome_versions_path(version = "latest", binary = "chrome", platform = NULL)

chrome_versions_add(version, binary, platform = NULL)

chrome_versions_remove(version, binary, platform = NULL, ask = TRUE)
```

## Arguments

- ...:

  Additional path parts.

- version:

  A character string specifying the version to list, add or remove.

- binary:

  A character string specifying which binary to list. Defaults to
  `"all"` to show all binaries, or can be one or more of of `"chrome"`,
  `"chrome-headless-shell"`, or `"chromedriver"`.

- platform:

  A character string specifying the platform(s) to list. If `NULL`
  (default), the platform will be automatically detected, or if `"all"`,
  then binaries for all platforms will be listed.

- ask:

  Whether to ask before removing files.

## Value

A character vector of Chrome binary paths.

## See also

[`chrome_versions_list()`](https://rstudio.github.io/chromote/dev/reference/chrome_versions_list.md)
