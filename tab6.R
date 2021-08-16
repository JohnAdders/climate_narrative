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
        asset = 3,
        1
      )
      print('DEBUG: updating tab 6 prev')
    }
  )
  observeEvent(
    input$wizard,
    {
      print(paste0('tab: ',input$wizard))
      if(input$wizard=='page_6'){
        print('DEBUG: updating final output')
        update_final_page(input, output, session)
      }
    }
  )
}