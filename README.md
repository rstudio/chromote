Chromote: Headless Chromium Remote Interface
============================================

Installation:

```R
devtools::install_github("rstudio/chromote")
```

Basic usage:


```R
library(chromote)

b <- Chromote$new()

# In a web browser, open a viewer for the headless browser
b$view()
```

The browser can be given _commands_, like this:

```R
b$Page$navigate("https://www.r-project.org/")
```

In the official Chrome DevTools Protocol documentation, this is the [`Page.navigate`](https://chromedevtools.github.io/devtools-protocol/1-3/Page#method-navigate) command.

These commands are executed asynchronously, so they do not return a value immediately; instead, after the browser executes the command, an optional callback function can be invoked, and it will be passed the value returned from the command. For example, if you'd like to see the returned value from the `Page.navigate` command, you can use the `str()` function as the callback. (Note that `str(x)` prints out the value of `x` to the R console.)

```R
b$Page$navigate("https://www.r-project.org/", cb_ = str)
```

This will wait a moment and then print out something like this:

```
List of 2
 $ frameId : chr "78F54D28A0C22A7EB9412D932985109E"
 $ loaderId: chr "88F2A0109AF319489827E23C5F3DB721"
```

This will query the browser for version information (using [`Browser.getVersion`](https://chromedevtools.github.io/devtools-protocol/tot/Browser#method-getVersion) method).

```R
# Get the version of the browser.
b$Browser$getVersion(cb_ = str)
```

```
List of 5
 $ protocolVersion: chr "1.3"
 $ product        : chr "HeadlessChrome/72.0.3626.81"
 $ revision       : chr "@ac8b982e05014492d1bd7d317628a4f22a97ffa0"
 $ userAgent      : chr "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/72.0.3626"| __truncated__
 $ jsVersion      : chr "7.2.502.24"
```

Take a screenshot and display it:

```R
b$Page$captureScreenshot(cb_ = function(msg) {
  tmp <- tempfile(fileext = ".png")
  writeBin(jsonlite::base64_dec(msg$data), tmp)
  message("Screenshot saved to ", tmp)
  on.exit(unlink(tmp))
  showimage::show_image(tmp)
})
```

See the [official Chrome DevTools Protocol documentation](https://chromedevtools.github.io/devtools-protocol/) for the full set of commands.

