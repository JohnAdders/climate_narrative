ui <- function() {
    tabset_start <- list(
        id = "wizard",
        type = "hidden"
    )
    tabset_tabs <- lapply(tabs, function(tab) {tab$ui()})
    fluidPage(
        useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "climate_narrative.css")
        ),
        useShinyjs(),
        GreCAPTCHAv3Ui(<your site key>,"homepage","responseReceived"),
        do.call(tabsetPanel, c(tabset_start, tabset_tabs)),
        theme = shinytheme("sandstone")
    )
}