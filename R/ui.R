ui <- function() {
  tabset_start <- list(
    id = "wizard",
    type = "hidden"
  )
  tabset_tabs <- lapply(global$tabs, function(tab) {
    tab$ui()
  })
  fluidPage(
    title = "Climate narrative",
    theme = shinytheme("sandstone"),
    tags$head(HTML("<base target=\"_blank\">")),
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "climate_narrative.css"),
    do.call(tabsetPanel, c(tabset_start, tabset_tabs)),
    textOutput("__heartbeat")
  )
}
