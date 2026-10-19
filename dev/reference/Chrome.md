# Local Chrome process

This is a subclass of
[`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)
that represents a local browser. It extends the
[`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)
class with a
[`processx::process`](http://processx.r-lib.org/reference/process.md)
object, which represents the browser's system process.

## See also

[`get_chrome_args()`](https://rstudio.github.io/chromote/dev/reference/default_chrome_args.md)

## Super class

[`Browser`](https://rstudio.github.io/chromote/dev/reference/Browser.md)
-\> `Chrome`

## Methods

### Public methods

- [`Chrome$new()`](#method-Chrome-initialize)

- [`Chrome$get_path()`](#method-Chrome-get_path)

- [`Chrome$clone()`](#method-Chrome-clone)

Inherited methods

- [`Browser$close()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-close)
- [`Browser$get_host()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-get_host)
- [`Browser$get_port()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-get_port)
- [`Browser$get_process()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-get_process)
- [`Browser$is_alive()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-is_alive)
- [`Browser$is_local()`](https://rstudio.github.io/chromote/dev/reference/Browser.html#method-is_local)

------------------------------------------------------------------------

### `Chrome$new()`

Create a new Chrome object.

#### Usage

    Chrome$new(path = find_chrome(), args = get_chrome_args())

#### Arguments

- `path`:

  Location of chrome installation

- `args`:

  A character vector of command-line arguments passed when initializing
  Chrome. Single on-off arguments are passed as single values
  (e.g.`"--disable-gpu"`), arguments with a value are given with a
  nested character vector (e.g. `c("--force-color-profile", "srgb")`).
  See
  [here](https://peter.sh/experiments/chromium-command-line-switches/)
  for a list of possible arguments. Defaults to
  [`get_chrome_args()`](https://rstudio.github.io/chromote/dev/reference/default_chrome_args.md).

#### Returns

A new `Chrome` object.

------------------------------------------------------------------------

### `Chrome$get_path()`

Browser application path

#### Usage

    Chrome$get_path()

------------------------------------------------------------------------

### `Chrome$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Chrome$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
