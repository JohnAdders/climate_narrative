
library(climate.narrative)

check_link_recursive <- function(arg) {
  if ("list" %in% is(arg)) {
    recursive <- lapply(arg, check_link_recursive)
    positive_length <- (sapply(recursive, length) > 0)
    return(recursive[positive_length])
  } else {
    arg <- strsplit(as.character(arg), "\n")
    arg <- unlist(arg)
    links <- arg[grep("http", arg)]
    n <- length(links)
    if (n > 0){
        good_links <- rep(FALSE, n)
        good_links <- good_links | grepl('{target="\\_blank"}', links, fixed = TRUE)
        good_links <- good_links | grepl('.pdf', links, fixed = TRUE)
        return(links[!good_links])
    } else {
        return(character(0))
    }
  }
}

testthat::test_that("all links that are no links to files (e.g. pdf) are open in the new tab", {
  expect_equal(length(check_link_recursive(global$exposure_classes)), 0)
})