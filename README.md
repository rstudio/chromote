
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Do not run R chunks that print any session information.
     This produces unstable output.
     Instead, copy output from a local execution
     Still use README.Rmd to get special UTF-8 chars from pandoc -->

# chromote <a href="https://rstudio.github.io/chromote/"><img src="man/figures/logo.png" align="right" height="138" alt="chromote website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/rstudio/chromote/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/chromote/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/chromote)](https://CRAN.R-project.org/package=chromote)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Chromote is an R implementation of the [Chrome DevTools
Protocol](https://chromedevtools.github.io/devtools-protocol/). It works
with Chrome, Chromium, Opera, Vivaldi, and other browsers based on
[Chromium](https://www.chromium.org/). By default it uses Google Chrome
(which must already be installed on the system). To use a different
browser, see `vignette("which-chrome")`.

Chromote is not the only R package that implements the Chrome DevTools
Protocol. Here are some others:

- [crrri](https://github.com/RLesur/crrri) by Romain Lesur and
  Christophe Dervieux
- [decapitated](https://github.com/hrbrmstr/decapitated/) by Bob Rudis
- [chradle](https://github.com/milesmcbain/chradle) by Miles McBain

The interface to Chromote is similar to
[chrome-remote-interface](https://github.com/cyrus-and/chrome-remote-interface)
for node.js.

## Features

- Install and use specific versions of Chrome from the [Chrome for
  Testing](https://googlechromelabs.github.io/chrome-for-testing/)
  service.

- Offers a synchronous API for ease of use and an asynchronous API for
  more sophisticated tasks.

- Full support for the Chrome DevTools Protocol for any version of
  Chrome or any Chrome-based browser.

- Includes convenience methods, like `$screenshot()` and
  `$set_viewport_size()`, for common tasks.

- Automatically reconnects to previous sessions if the connection from R
  to Chrome is lost, for example when restarting from sleep state.

- Powers many higher-level packages and functions, like `{shinytest2}`
  and `rvest::read_html_live()`.

## Learn More

Learn more about using and programming with chromote:

- [Get
  started](https://rstudio.github.io/chromote/articles/chromote.html)
- [Commands and
  events](https://rstudio.github.io/chromote/articles/commands-and-events.html)
- [Synchronous vs. asynchronous
  usage](https://rstudio.github.io/chromote/articles/sync-async.html)
- [Choosing which Chrome-based browser to
  use](https://rstudio.github.io/chromote/articles/which-chrome.html)

## Installation

Install the released version of chromote from CRAN:

``` r
install.packages("chromote")
```

Or install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("rstudio/chromote")
```

## Basic usage

This will start a headless browser and open an interactive viewer for it
in a normal browser, so that you can see what the headless browser is
doing.

``` r
library(chromote)

b <- ChromoteSession$new()

# In a web browser, open a viewer for the headless browser. Works best with
# Chromium-based browsers.
b$view()
```

The browser can be given *commands*, as specified by the [Chrome
DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/).
For example, `$Browser$getVersion()` (which corresponds to the
[Browser.getVersion](https://chromedevtools.github.io/devtools-protocol/tot/Browser/#method-getVersion)
in the API docs) will query the browser for version information:

``` r
b$Browser$getVersion()
#> $protocolVersion
#> [1] "1.3"
#>
#> $product
#> [1] "HeadlessChrome/98.0.4758.102"
#>
#> $revision
#> [1] "@273bf7ac8c909cde36982d27f66f3c70846a3718"
#>
#> $userAgent
#> [1] "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/98.0.4758.102 Safari/537.36"
#>
#> $jsVersion
#> [1] "9.8.177.11"
```

If you have the viewer open and run the following, you’ll see the web
page load in the viewer[^1]:

``` r
b$go_to("https://www.r-project.org/")
```

In addition to full support of the Chrome Devtools Protocol,
`ChromoteSession` objects also have some convenience methods, like
`$go_to()` and `$screenshot()`. (See the Examples section below for more
information about screenshots.)

``` r
# Saves to screenshot.png
b$screenshot()

# Takes a screenshot of elements picked out by CSS selector
b$screenshot("sidebar.png", selector = ".sidebar")
```

<figure>
<img src="man/figures/sidebar.png"
alt="A screenshot of the sidebar of r-rproject.org, circa 2023." />
<figcaption aria-hidden="true">A screenshot of the sidebar of
r-rproject.org, circa 2023.</figcaption>
</figure>

[^1]: This simple example works interactively, but if you’re using
    chromote to programmatically take screenshots you’ll want to read
    `vignette("example-loading-page")` for a consistent and reliable
    approach.
