tab_title_ui <- function() {
  list(
    img(src = "climate_narrative/title.png", alt = "CFRF logo4", width = "70%")
  )
}

tab_title_server <- function(input, output, session, tab) {
  if (global$dev == TRUE){
    tab$next_tab <- tab_name_to_number("type")
  }
}
