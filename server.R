server <- function(input, output, session) {
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }

  for (tab in tabs) {
    tab$server(input, output, session, switch_page)
  }

  
}