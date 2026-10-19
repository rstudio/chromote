# Remote Chrome process

Remote Chrome process

## Super class

[`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)
-\> `ChromeRemote`

## Methods

### Public methods

- [`ChromeRemote$new()`](#method-ChromeRemote-initialize)

- [`ChromeRemote$is_alive()`](#method-ChromeRemote-is_alive)

- [`ChromeRemote$close()`](#method-ChromeRemote-close)

- [`ChromeRemote$clone()`](#method-ChromeRemote-clone)

Inherited methods

- [`Browser$get_host()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-get_host)
- [`Browser$get_port()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-get_port)
- [`Browser$get_process()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-get_process)
- [`Browser$is_local()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-is_local)

------------------------------------------------------------------------

### `ChromeRemote$new()`

Create a new ChromeRemote object.

#### Usage

    ChromeRemote$new(host, port)

#### Arguments

- `host`:

  A string that is a valid IPv4 or IPv6 address. `"0.0.0.0"` represents
  all IPv4 addresses and `"::/0"` represents all IPv6 addresses.

- `port`:

  A number or integer that indicates the server port.

------------------------------------------------------------------------

### `ChromeRemote$is_alive()`

Is the remote service alive?

#### Usage

    ChromeRemote$is_alive()

------------------------------------------------------------------------

### `ChromeRemote$close()`

chromote does not manage remote processes, so closing a remote Chrome
browser does nothing. You can send a `Browser$close()` command if this
is really something you want to do.

#### Usage

    ChromeRemote$close()

------------------------------------------------------------------------

### `ChromeRemote$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ChromeRemote$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
