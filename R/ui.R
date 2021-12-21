#' Main Shiny User Interface function
#'
#' @importFrom tippy use_tippy
#' @importFrom shinyjs useShinyjs
#' @importFrom shinythemes shinytheme
#'
#' @export
#'
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
    use_tippy(),
    useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "/climate_narrative/climate_narrative.css"),
    do.call(tabsetPanel, c(tabset_start, tabset_tabs)),
    textOutput("__heartbeat")
  )
}
