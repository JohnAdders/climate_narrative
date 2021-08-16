ui <- function() {
  tabset_start <- list(
    id = "wizard",
    type = "hidden"
  )

  tabset_tabs <- lapply(tabs, function(tab) {tab$ui()})

  fluidPage(
    do.call(tabsetPanel, c(tabset_start, tabset_tabs))
  )
}