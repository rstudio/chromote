library(testthat)
library(chromote)

on_cran <- !interactive() && isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))

if (!on_cran && !identical(Sys.getenv("CHROMOTE_CHROME"), "")) {
  path <- chrome_versions_add("latest-stable", "chrome")
  Sys.setenv("CHROMOTE_CHROME" = path)
}

test_check("chromote")
