library(testthat)
library(climate.narrative)
test_check("climate.narrative")
#
# previous code, based on https://shiny.rstudio.com/articles/testing-overview.html
#
# test_dir(
#   "./testthat",
#   env = shiny::loadSupport(),
#   reporter = c("progress", "fail")
# )
