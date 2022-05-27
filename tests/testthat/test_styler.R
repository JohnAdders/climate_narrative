library(climate.narrative)
library(styler)
testthat::test_that("sources files are nicely styled", {
  out <- styler::style_pkg(system.file(package = "climate.narrative"), dry = "on")

  expect_equal(sum(out$changed), 0)
})
