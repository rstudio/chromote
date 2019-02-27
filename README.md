Chromote: Headless Chromium Remote Interface
============================================

**Please note that Chromote is in development and the API is subject to change**

## Installation

```R
devtools::install_github("rstudio/chromote")
```

## Basic usage


```R
library(chromote)

b <- Chromote$new()

# In a web browser, open a viewer for the headless browser
b$view()
```

The browser can be given _commands_, as specified by the [Chrome Devtools Protocol](https://chromedevtools.github.io/devtools-protocol/). For example, `$Browser$getVersion()` (which corresponds to the [Browser.getVersion](https://chromedevtools.github.io/devtools-protocol/tot/Browser#method-getVersion) in the API docs) will query the browser for version information:


```R
b$Browser$getVersion()
#> $protocolVersion
#> [1] "1.3"
#>
#> $product
#> [1] "HeadlessChrome/72.0.3626.109"
#>
#> $revision
#> [1] "@fae8db7ab9280fa6704a59980263c804f809ebd5"
#>
#> $userAgent
#> [1] "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) #> HeadlessChrome/72.0.3626.109 Safari/537.36"
#>
#> $jsVersion
#> [1] "7.2.502.25"
```


If you have the viewer open and run the following, you'll see the web page load in the viewer:

```R
b$Page$navigate("https://www.r-project.org/")
```

In the official Chrome DevTools Protocol documentation, this is the [`Page.navigate`](https://chromedevtools.github.io/devtools-protocol/1-3/Page#method-navigate) command.


In addition to full support of the Chrome Devtools Protocol, Chromote objects also some convenience methods, like `$screenshot()`. (See the Examples section below for more information about screenshots.)

```R
b$screenshot()  # Saves to screenshot.png and displays in viewer
```

This will get the dimensions of a DOM element given a CSS selector:

```R
x <- b$DOM$getDocument()
x <- b$DOM$querySelector(x$root$nodeId, ".sidebar")
x <- b$DOM$getBoxModel(x$nodeId)
str(x)
#> List of 1
#>  $ model:List of 6
#>   ..$ content:List of 8
#>   .. ..$ : int 92
#>   .. ..$ : int 28
#>   .. ..$ : int 187
#>   .. ..$ : int 28
#>   .. ..$ : int 187
#>   .. ..$ : int 1026
#>   .. ..$ : int 92
#>   .. ..$ : int 1026
#>         ...
#>         ...
#>   ..$ margin :List of 8
#>   .. ..$ : num 14.5
#>   .. ..$ : int 28
#>   .. ..$ : int 202
#>   .. ..$ : int 28
#>   .. ..$ : int 202
#>   .. ..$ : int 1068
#>   .. ..$ : num 14.5
#>   .. ..$ : int 1068
#>   ..$ width  : int 125
#>   ..$ height : int 998
```

Or you can do the same thing by chaining commands together with a magrittr pipe:

```R
library(magrittr)

b$DOM$getDocument() %>%
  { b$DOM$querySelector(.$root$nodeId, ".sidebar") } %>%
  { b$DOM$getBoxModel(.$nodeId) } %>%
  str()
```


### Synchronous and asynchronous usage

By default, a Chromote object operates in _synchronous_ mode: when you call a comnmand function (like `b$Page$navigate()`), a command is sent to the headless browser, the headless browser executes that command, and it sends a response message back. When the R process receives the response, it converts it from JSON to an R object and the function returns that value. During this time, the R process is blocked; no other R code can execute.

The Chromote object can be put into _asynchronous_ mode. In async mode, a command function fires off a message to the browser, and then the R process continues running code; when the response comes back at some time in the future, the R process can pass the value to another function. This is implemented using [promises](https://rstudio.github.io/promises/) (note that these are not the regular R-language promises; these are similar to JavaScript promises for async programming.)


To switch to async mode:

```R
b <- Chromote$new()
b$sync_mode(FALSE)  # Switch to async mode
```

In async mode, there are two ways to use the returned value. One is to use the `callback_` argument. For example:

```R
# The str function prints out the value, nicely formatted
b$Browser$getVersion(callback_ = str)
#> List of 5
#>  $ protocolVersion: chr "1.3"
#>  $ product        : chr "HeadlessChrome/72.0.3626.109"
#>  $ revision       : chr "@fae8db7ab9280fa6704a59980263c804f809ebd5"
#>  $ userAgent      : chr "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like #> Gecko) HeadlessChrome/72.0.3626.109 Safari/537.36"
#>  $ jsVersion      : chr "7.2.502.25"


# This will extract the product field
product <- NULL
b$Browser$getVersion(callback_ = function(msg) {
  product <<- msg$product
})
```

The second way is to use _promises_. If no `callback_` is passed to the command, then it will return a promise. Promises have many advantages over plain old callbacks: they are easier to chain, and they provide better error-handling capabilities.

Here's an example that uses promises to print out the version information. Note that the surrounding curly braces are there to indicate that this whole thing must be run as a block without any idle time in between the function calls -- if you were to run the code in the R console line-by-line, the browser would send back the message and the promise would resolve before you called `p$then()`, which is where you tell the promise what to do with the return value. (The curly braces aren't strictly necessary -- you could run the code inside the braces in a single paste operation and have the same effect.)

```R
{
  p <- b$Browser$getVersion()
  p$then(function(value) {
    print(value$product)
  })
}
```

Here are some progressively more concise ways of achieving the same thing. As you work with promises, you will see these various forms of promise chaining.

```R
library(promises)

# Regular function pipe to then()
b$Browser$getVersion() %>% then(function(value) {
  print(value$product)
})

# Promise-pipe to anonymous function
b$Browser$getVersion() %...>% (function(value) {
  print(value$product)
})

# Promise-pipe to an expression (which gets converted to a function with the first argument `.`)
b$Browser$getVersion() %...>% { print(.$product) }

# Promise-pipe to a named function, with parentheses
print_product <- function(msg) print(msg$product)
b$Browser$getVersion() %...>% print_product()

# Promise-pipe to a named function, without parentheses
b$Browser$getVersion() %...>% print_product
```


The earlier example where we found the dimensions of a DOM element using CSS selectors can be done in async mode by switching from the regular pipe to the promise-pipe:

```R
b$DOM$getDocument() %...>%
  { b$DOM$querySelector(.$root$nodeId, ".sidebar") } %...>%
  { b$DOM$getBoxModel(.$nodeId) } %...>%
  str()
```

There may be times where the Chromote object is in async mode and you want to wait for a promise to resolve before continuing. This can be useful when you are switching from async mode to synchronous mode -- you may want to make sure some work is completed before switching modes. To do this, you can use the `wait_for()` method.

```R
# A promise chain
p <- b$DOM$getDocument() %...>%
  { b$DOM$querySelector(.$root$nodeId, ".sidebar") } %...>%
  { b$DOM$getBoxModel(.$nodeId) } %...>%
  str()

b$wait_for(p)
```


**Technical note about the event loop**: In async mode, the R process will run callbacks and promises using an event loop provided by the [later](https://github.com/r-lib/later) package. This event loop is very similar to the one used in JavaScript, which is explained in depth by [this article](https://blog.sessionstack.com/how-javascript-works-event-loop-and-the-rise-of-async-programming-5-ways-to-better-coding-with-2f077c4438b5). One important difference between JavaScript's event loop and the one provided by **later**'s is that in JavaScript, the event loop only runs when the call stack is empty (essentially, when the JS runtime is idle); with **later** the event loop similarly runs when the call stack is empty (when the console is idle), but it can also be run at any point by calling `later::run_now()`.


## Events

In addition to _commands_ The Chrome Devtools Protocol also has _events_. These are messages that are sent from the browser to the R process when various browser events happen.

As an example, it can be a bit tricky to find out when to take a screenshot. When you send the browser a command to navigate to a page, it sends a response immediately, but it may take several more seconds for it to actually load that page. When it does, the `Page.loadEventFired` event will be triggered.

```R
b <- Chromote$new()

# Tell browser to send event notifications for the Page domain
b$Page$enable()


# Navigate and wait for Page.loadEventFired.
# Note: these lines must be run without any delay in between
b$Page$navigate("https://www.r-project.org/")
b$Page$loadEventFired()
```

TODO: Add async events

TODO: Add way to print out all events


## Chrome on remote hosts

Chromote can control a browser running on a remote host. To start the browser, open a terminal on the remote host and run:

```
"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless --remote-debugging-address=0.0.0.0 --remote-debugging-port=9222
```

(The path to Chrome is for Mac. On Windows or Linux, it will be different.)

Then, in your local R session, create a Chromote object with the `host` and `port`. Once it's created, you can control it the same as usual:

```R
r <- Chromote$new(
  browser = ChromeRemote$new(host = "10.0.0.5", port = 9222)
)

r$Browser$getVersion()
r$view()
r$Page$navigate("https://www.whatismybrowser.com/")
b$Page$loadEventFired()
r$screenshot()
r$screenshot(".string-major")
```

When you use `$view()` on the remote browser, your local browser may block scripts for security reasons, which means that you won't be able to view the remote browser. If your local browser is Chrome, there will be a shield-shaped icon in the location bar that you can click in order to enable loading the scripts. (Note: I haven't been able to get other local browsers to work at all.)

Note: There seem to be some timing issues with remote browsers. In the example above, the browser may finish navigating to the web site before the R process gets the response message for `$navigate()`, and therefore before it starts waiting for `Page.loadEventFired`. We'll work on smoothing this over.

*****

## Examples

Take a screenshot of the viewport and display it using the [showimage](https://github.com/r-lib/showimage#readme) package.

```R
b <- Chromote$new()
b$Page$enable()

b$Page$navigate("https://www.r-project.org/")
b$Page$loadEventFired()
b$screenshot()  # Saves to screenshot.png and displays in viewer


# Using CSS selectors, choosing the region, and using scaling
b$Page$navigate("https://www.reddit.com")
b$Page$loadEventFired()
b$screenshot('#header-search-bar', region = 'content')
b$screenshot('#header-search-bar', region = 'padding')
b$screenshot('#header-search-bar', region = 'border')
b$screenshot('#header-search-bar', region = 'border', scale = 2)
```
