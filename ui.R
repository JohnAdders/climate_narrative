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
        uiOutput(outputId = "display_content_basic"),
        theme = shinytheme("sandstone")
    )
}