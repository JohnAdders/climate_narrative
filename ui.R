ui <- function() {
  tabset_start <- list(
    id = "wizard",
    type = "hidden"
  )

  tabset_tabs <- lapply(tabs, function(tab) {tab$ui()})
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "climate_narrative.css")
    ),
    do.call(tabsetPanel, c(tabset_start, tabset_tabs))
  )
}