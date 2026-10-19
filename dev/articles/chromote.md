# chromote

Chromote is an R implementation of the [Chrome DevTools
Protocol](https://chromedevtools.github.io/devtools-protocol/). It works
with Chrome, Chromium, Opera, Vivaldi, and other browsers based on
[Chromium](https://www.chromium.org/). By default it uses Google Chrome
(which must already be installed on the system). To use a different
browser, see
[`vignette("which-chrome")`](https://rstudio.github.io/chromote/dev/articles/which-chrome.md).

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

- Powers many higher-level packages and functions, like
  [shinytest2](https://rstudio.github.io/shinytest2/) and
  `rvest::read_html_live()`.

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

![A screenshot of the sidebar of r-rproject.org, circa
2023.](../reference/figures/sidebar.png)

A screenshot of the sidebar of r-rproject.org, circa 2023.

> **Technical Note**
>
> All members of `Chromote` and `ChromoteSession` objects which start
> with a capital letter (like `b$Page`, `b$DOM`, and `b$Browser`)
> correspond to domains from the Chrome DevTools Protocol, and are
> documented in the [official CDP
> site](https://chromedevtools.github.io/devtools-protocol/). All
> members which start with a lower-case letter (like `b$screenshot` and
> `b$close`) are not part of the Chrome DevTools Protocol, and are
> specific to `Chromote` and `ChromoteSession`.

Here is an example of how to use Chromote to find the position of a DOM
element using
[DOM.getBoxModel](https://chromedevtools.github.io/devtools-protocol/tot/DOM/#method-getBoxModel).

``` r

x <- b$DOM$getDocument()
x <- b$DOM$querySelector(x$root$nodeId, ".sidebar")
x <- b$DOM$getBoxModel(x$nodeId)
str(x)
#> List of 1
#>  $ model:List of 6
#>   ..$ content:List of 8
#>   .. ..$ : num 128
#>   .. ..$ : int 28
#>   .. ..$ : num 292
#>   .. ..$ : int 28
#>   .. ..$ : num 292
#>   .. ..$ : num 988
#>   .. ..$ : num 128
#>   .. ..$ : num 988
#>   ..$ padding:List of 8
#>   .. ..$ : num 112
#>   .. ..$ : int 28
#>   .. ..$ : num 308
#>   .. ..$ : int 28
#>   .. ..$ : num 308
#>   .. ..$ : num 988
#>   .. ..$ : num 112
#>   .. ..$ : num 988
#>   ..$ border :List of 8
#>   .. ..$ : num 112
#>   .. ..$ : int 28
#>   .. ..$ : num 308
#>   .. ..$ : int 28
#>   .. ..$ : num 308
#>   .. ..$ : num 988
#>   .. ..$ : num 112
#>   .. ..$ : num 988
#>   ..$ margin :List of 8
#>   .. ..$ : int 15
#>   .. ..$ : int 28
#>   .. ..$ : num 308
#>   .. ..$ : int 28
#>   .. ..$ : num 308
#>   .. ..$ : num 1030
#>   .. ..$ : int 15
#>   .. ..$ : num 1030
#>   ..$ width  : int 195
#>   ..$ height : int 960
```

## Creating new tabs and managing the process

To create a new tab/window:

``` r

b1 <- b$new_session()
```

Once it’s created, you can perform operations with the new tab without
affecting the first one.

``` r

b1$view()
b1$Page$navigate("https://github.com/rstudio/chromote")
#> $frameId
#> [1] "714439EBDD663E597658503C86F77B0B"
#>
#> $loaderId
#> [1] "F39339CBA7D1ACB83618FEF40C3C7467"
```

To close a browser tab/window, you can run:

``` r

b1$close()
```

This is different from shutting down the browser process. If you call
`b$close()`, the browser process will still be running, even if all tabs
have been closed. If all tabs have been closed, you can still create a
new tab by calling `b1$new_session()`.

To shut down the process, call:

``` r

b1$parent$close()
```

`b1$parent` is a `Chromote` object (as opposed to `ChromoteSession`),
which represents the browser as a whole. This is explained in [The
Chromote object model](#the-chromote-object-model).

## Commands and Events

The Chrome DevTools Protocol has two types of methods: *commands* and
*events*. The methods used in the previous examples are commands. That
is, they tell the browser to do something; the browser does it, and then
sends back some data. Learn more in
[`vignette("commands-and-events")`](https://rstudio.github.io/chromote/dev/articles/commands-and-events.md).

## The Chromote object model

There are two R6 classes that are used to represent the Chrome browser.
One is `Chromote`, and the other is `ChromoteSession`. A `Chromote`
object represents the browser as a whole, and it can have multiple
*targets*, which each represent a browser tab. In the Chrome DevTools
Protocol, each target can have one or more debugging *sessions* to
control it. A `ChromoteSession` object represents a single *session*.

When a `ChromoteSession` object is instantiated, a target is created,
then a session is attached to that target, and the `ChromoteSession`
object represents the session. (It is possible, though not very useful,
to have multiple `ChromoteSession` objects connected to the same target,
each with a different session.)

A `Chromote` object can have any number of `ChromoteSession` objects as
children. It is not necessary to create a `Chromote` object manually.
You can simply call:

``` r

b <- ChromoteSession$new()
```

and it will automatically create a `Chromote` object if one has not
already been created. The Chromote package will then designate that
`Chromote` object as the *default* Chromote object for the package, so
that any future calls to `ChromoteSession$new()` will automatically use
the same `Chromote`. This is so that it doesn’t start a new browser for
every `ChromoteSession` object that is created.

In the Chrome DevTools Protocol, most commands can be sent to individual
sessions using the `ChromoteSession` object, but there are some commands
which can only be sent to the overall browser, using the `Chromote`
object.

To access the parent `Chromote` object from a `ChromoteSession`, you can
simply use `$parent`:

``` r

b <- ChromoteSession$new()
m <- b$parent
```

With a `Chromote` object, you can get a list containing all the
`ChromoteSession`s, with `$get_sessions()`:

``` r

m$get_sessions()
```

Normally, subsequent calls to `ChromoteSession$new()` will use the
existing `Chromote` object. However, if you want to start a new browser
process, you can manually create a `Chromote` object, then spawn a
session from it; or you can pass the new `Chromote` object to
`ChromoteSession$new()`:

``` r

cm <- Chromote$new()
b1 <- cm$new_session()

# Or:
b1 <- ChromoteSession$new(parent = cm)
```

Note that if you use either of these methods, the new `Chromote` object
`cm` will *not* be set as the default that is used by future calls to
`ChromoteSesssion$new()`. See
[`vignette("which-chrome")`](https://rstudio.github.io/chromote/dev/articles/which-chrome.md)
for an example showing how you can set the default `Chromote` object.

There are also the following classes which represent the browser at a
lower level:

- `Browser`: This represents an instance of a browser that supports the
  Chrome DevTools Protocol. It contains information about how to
  communicate with the Chrome browser. A `Chromote` object contains one
  of these.
- `Chrome`: This is a subclass of `Browser` that represents a local
  browser. It extends the `Browser` class with a
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  object, which represents the browser’s system process.
- `ChromeRemote`: This is a subclass of `Browser` that represents a
  browser running on a remote system.

## Debugging

Calling `b$debug_messages(TRUE)` will enable the printing of all the
JSON messages sent between R and Chrome. This can be very helpful for
understanding how the Chrome DevTools Protocol works.

``` r

b <- ChromoteSession$new()
b$parent$debug_messages(TRUE)
b$Page$navigate("https://www.r-project.org/")
#> SEND {"method":"Page.navigate","params":{"url":"https://www.r-project.org/"},"id":53,"sessionId":"12CB6B044A379DA0BDCFBBA55318247C"}
#> $frameId
#> [1] "BAAC175C67E55886207BADE1776E7B1F"
#>
#> $loaderId
#> [1] "66DED3DF9403DA4A307444765FDE828E"

# Disable debug messages
b$parent$debug_messages(FALSE)
```

## Resource cleanup and garbage collection

When Chromote starts a Chrome process, it calls `Chrome$new()`. This
launches the Chrome process it using
[`processx::process()`](http://processx.r-lib.org/reference/process.md),
and enables a supervisor for the process. This means that if the R
process stops, the supervisor will detect this and shut down any Chrome
processes that were registered with the supervisor. This prevents the
proliferation of Chrome processes that are no longer needed.

The Chromote package will, by default, use a single Chrome process and a
single `Chromote` object, and each time `ChromoteSession$new()` is
called, it will spawn them from the `Chromote` object. See [The Chromote
object model](#the-chromote-object-model) for more information.

[^1]: This simple example works interactively, but if you’re using
    chromote to programmatically take screenshots you’ll want to read
    [`vignette("example-loading-page")`](https://rstudio.github.io/chromote/dev/articles/example-loading-page.md)
    for a consistent and reliable approach.
