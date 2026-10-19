# Default Chromote object

Returns the Chromote package's default
[Chromote](https://rstudio.github.io/chromote/dev/reference/Chromote.md)
object. If there is not currently a default `Chromote` object that is
active, then one will be created and set as the default.

## Usage

``` r
default_chromote_object()

has_default_chromote_object()

set_default_chromote_object(x)
```

## Arguments

- x:

  A
  [Chromote](https://rstudio.github.io/chromote/dev/reference/Chromote.md)
  object.

## Details

`ChromoteSession$new()` calls this function by default, if the `parent`
is not specified. That means that when `ChromoteSession$new()` is called
and there is not currently an active default `Chromote` object, then a
new `Chromote` object will be created and set as the default.
