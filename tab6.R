tab6_ui <- function () {
  list(
    h3('INFO: Aggregated inputs (ordered by materiality)') ,
    p('It will be used to derive the narrative'),
    tableOutput("show_aggregated_inputs"),
    uiOutput("summary")
  )
}

tab6_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- switch(
        input$type,
        insurance = 5,
        asset = 3,
        1
      )
    }
  )
  observeEvent(
    input$wizard,
    if(input$wizard=='page_6') update_final_page(input, output, session)
  )
}