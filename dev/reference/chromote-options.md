# chromote Options

These options and environment variables that are used by chromote.
Options are lowercase and can be set with
[`options()`](https://rdrr.io/r/base/options.html). Environment
variables are uppercase and can be set in an `.Renviron` file, with
[`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html), or in the
shell or process running R. If both an option or environment variable
are supported, chromote will use the option first.

- `CHROMOTE_CHROME`  
  Path to the Chrome executable. If not set, chromote will attempt to
  find and use the system installation of Chrome.

- `chromote.headless`, `CHROMOTE_HEADLESS`  
  Headless mode for Chrome. Can be `"old"` or `"new"`. See [Chrome
  Headless
  mode](https://developer.chrome.com/docs/chromium/new-headless) for
  more details.

- `chromote.timeout`  
  Timeout (in seconds) for Chrome to launch or connect. Default is `10`.

- `chromote.launch.echo_cmd`  
  Echo the command used to launch Chrome to the console for debugging.
  Default is `FALSE`.
