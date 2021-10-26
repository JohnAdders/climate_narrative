ui <- function() {
    fluidPage(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "climate_narrative.css")
        ),
        uiOutput(outputId = "display_content_basic"),
        theme = shinytheme("sandstone")
    )
}