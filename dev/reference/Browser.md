# Browser base class

Base class for browsers like Chrome, Chromium, etc. Defines the
interface used by various browser implementations. It can represent a
local browser process or one running remotely.

## Details

The `initialize()` method of an implementation should set `private$host`
and `private$port`. If the process is local, the `initialize()` method
should also set `private$process`.

## Methods

### Public methods

- [`Browser$is_local()`](#method-Browser-is_local)

- [`Browser$get_process()`](#method-Browser-get_process)

- [`Browser$is_alive()`](#method-Browser-is_alive)

- [`Browser$get_host()`](#method-Browser-get_host)

- [`Browser$get_port()`](#method-Browser-get_port)

- [`Browser$close()`](#method-Browser-close)

- [`Browser$clone()`](#method-Browser-clone)

------------------------------------------------------------------------

### `Browser$is_local()`

Is local browser? Returns TRUE if the browser is running locally, FALSE
if it's remote.

#### Usage

    Browser$is_local()

------------------------------------------------------------------------

### `Browser$get_process()`

Browser process

#### Usage

    Browser$get_process()

------------------------------------------------------------------------

### `Browser$is_alive()`

Is the process alive?

#### Usage

    Browser$is_alive()

------------------------------------------------------------------------

### `Browser$get_host()`

Browser Host

#### Usage

    Browser$get_host()

------------------------------------------------------------------------

### `Browser$get_port()`

Browser port

#### Usage

    Browser$get_port()

------------------------------------------------------------------------

### `Browser$close()`

Close the browser

#### Usage

    Browser$close(wait = FALSE)

#### Arguments

- `wait`:

  If an integer, waits a number of seconds for the process to exit,
  killing the process if it takes longer than `wait` seconds to close.
  Use `wait = TRUE` to wait for 10 seconds.

------------------------------------------------------------------------

### `Browser$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Browser$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
