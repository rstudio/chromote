
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Do not run R chunks that print any session information.
     This produces unstable output.
     Instead, copy output from a local execution
     Still use README.Rmd to get special UTF-8 chars from pandoc -->

# Chromote: Headless Chrome Remote Interface

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
browser, see [Specifying which browser to
use](#specifying-which-browser-to-use).

Chromote is not the only R package that implements the Chrome DevTools
Protocol. Here are some others:

- [crrri](https://github.com/RLesur/crrri) by Romain Lesur and
  Christophe Dervieux
- [decapitated](https://github.com/hrbrmstr/decapitated/) by Bob Rudis
- [chradle](https://github.com/milesmcbain/chradle) by Miles McBain

The interface to Chromote is similar to
[chrome-remote-interface](https://github.com/cyrus-and/chrome-remote-interface)
for node.js.

## Installation

``` r
# CRAN
install.packages("chromote")

# Development
remotes::install_github("rstudio/chromote")
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
page load in the viewer:

``` r
b$Page$navigate("https://www.r-project.org/")
```

In the official Chrome DevTools Protocol (CDP) documentation, this is
the
[`Page.navigate`](https://chromedevtools.github.io/devtools-protocol/tot/Page/#method-navigate)
command.

In addition to full support of the CDP, `ChromoteSession` objects also
some convenience methods, like `$screenshot()`. (See the Examples
section below for more information about screenshots.)

``` r
# Saves to screenshot.png
b$screenshot()

# Takes a screenshot of elements picked out by CSS selector
b$screenshot("sidebar.png", selector = ".sidebar")
```

![](man/figures/sidebar.png)
