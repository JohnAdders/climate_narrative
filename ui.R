ui <- function() {
    tabset_start <- list(
        id = "wizard",
        type = "hidden"
    )
    tabset_tabs <- lapply(tabs, function(tab) {tab$ui()})
    fluidPage(
        title='Climate narrative',
        useShinyjs(),
        tags$link(rel = "stylesheet", type = "text/css", href = "climate_narrative.css"),
        tag('header',list(
            img(src='cfrf_logo.png', alt='CFRF logo', height=50),
            p('Climate Financial Risk Forum, 2021')
          )),
        hr(),
        do.call(tabsetPanel, c(tabset_start, tabset_tabs)),
        theme = shinytheme("sandstone"),
        heartbeat_footer()
    )
}