tab6_ui <- function () {
  list(
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
        asset = 4,
        2
      )
    }
  )
  observeEvent(
    input$wizard,
    if(input$wizard=='page_6') update_final_page(input, output, session)
  )
}