server <- function(input, output, session) {
  for (tab in tabs) {
    tab$server(input, output, session, switch_page)
  }
}