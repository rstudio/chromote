on_cran <- !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))

if (!on_cran) {
  has_chromote_envvar <- !identical(Sys.getenv("CHROMOTE_CHROME"), "")

  if (!has_chromote_envvar) {
    local_chrome_version("latest-stable", "chrome")
  }
}
