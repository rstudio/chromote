# Show information about the chromote package and Chrome browser

This function gathers information about the operating system, R version,
chromote package version, environment variables, Chrome path, and Chrome
arguments. It also verifies the Chrome installation and retrieves its
version.

## Usage

``` r
chromote_info()
```

## Value

A list containing the following elements:

- os:

  The operating system platform.

- version_r:

  The version of R.

- version_chromote:

  The version of the chromote package.

- envvar:

  The value of the `CHROMOTE_CHROME` environment variable.

- path:

  The path to the Chrome browser.

- args:

  A vector of Chrome arguments.

- version:

  The version of Chrome (if verification is successful).

- error:

  The error message (if verification fails).

- .check:

  A list with the status and output of the Chrome verification.

## Examples

``` r
chromote_info()
#> ---- {chromote} ----
#>    System: x86_64-pc-linux-gnu
#> R version: R version 4.6.1 (2026-06-24)
#>  chromote: 0.5.1.9000 (local::.)
#> 
#> ---- Chrome ----
#>    Path: /usr/bin/google-chrome
#> Version: Google Chrome 155.0.8059.39
#>    Args: --headless --no-sandbox --disable-dev-shm-usage
#>          --force-color-profile=srgb --disable-extensions --mute-audio
```
