# Default Chrome arguments

A character vector of command-line arguments passed when initializing
any new instance of
[`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md).
Single on-off arguments are passed as single values
(e.g.`"--disable-gpu"`), arguments with a value are given with a nested
character vector (e.g. `c("--force-color-profile", "srgb")`). See
[here](https://peter.sh/experiments/chromium-command-line-switches/) for
a list of possible arguments.

## Usage

``` r
default_chrome_args()

get_chrome_args()

set_chrome_args(args)
```

## Arguments

- args:

  A character vector of command-line arguments (or `NULL`) to be used
  with every new
  [`ChromoteSession`](https://rstudio.github.io/chromote/dev/reference/ChromoteSession.md).

## Value

A character vector of default command-line arguments to be used with
every new
[`ChromoteSession`](https://rstudio.github.io/chromote/dev/reference/ChromoteSession.md)

## Details

Default chromote arguments are composed of the following values (when
appropriate):

- [`"--disable-gpu"`](https://peter.sh/experiments/chromium-command-line-switches/#disable-gpu)

  - Only added on Windows, as empirically it appears to be needed (if
    not, check runs on GHA never terminate).

  - Disables GPU hardware acceleration. If software renderer is not in
    place, then the GPU process won't launch.

- [`"--no-sandbox"`](https://peter.sh/experiments/chromium-command-line-switches/#no-sandbox)

  - Only added when `CI` system environment variable is set, when the
    user on a Linux system is not set, or when executing inside a Docker
    container.

  - Disables the sandbox for all process types that are normally
    sandboxed. Meant to be used as a browser-level switch for testing
    purposes only

- [`"--disable-dev-shm-usage"`](https://peter.sh/experiments/chromium-command-line-switches/#disable-dev-shm-usage)

  - Only added when `CI` system environment variable is set or when
    inside a docker instance.

  - The `/dev/shm` partition is too small in certain VM environments,
    causing Chrome to fail or crash.

- [`"--force-color-profile=srgb"`](https://peter.sh/experiments/chromium-command-line-switches/#force-color-profile)

  - This means that screenshots taken on a laptop plugged into an
    external monitor will often have subtly different colors than one
    taken when the laptop is using its built-in monitor. This problem
    will be even more likely across machines.

  - Force all monitors to be treated as though they have the specified
    color profile.

- [`"--disable-extensions"`](https://peter.sh/experiments/chromium-command-line-switches/#disable-extensions)

  - Disable extensions.

- [`"--mute-audio"`](https://peter.sh/experiments/chromium-command-line-switches/#mute-audio)

  - Mutes audio sent to the audio device so it is not audible during
    automated testing.

## Functions

- `default_chrome_args()`: Returns a character vector of command-line
  arguments passed when initializing Chrome. See Details for more
  information.

- `get_chrome_args()`: Retrieves the default command-line arguments
  passed to
  [`Chrome`](https://rstudio.github.io/chromote/dev/reference/Chrome.md)
  during initialization. Returns either `NULL` or a character vector.

- `set_chrome_args()`: Sets the default command-line arguments passed
  when initializing. Returns the updated defaults.

## Examples

``` r
old_chrome_args <- get_chrome_args()

# Disable the gpu and use of `/dev/shm`
set_chrome_args(c("--disable-gpu", "--disable-dev-shm-usage"))

#... Make new `Chrome` or `ChromoteSession` instance

# Restore old defaults
set_chrome_args(old_chrome_args)
```
