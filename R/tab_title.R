tab_title_ui <- function() {
  out <- list(
    img(src = "climate_narrative/title.png", alt = "CFRF logo4", width = "70%")
  )
  if (global$dev){
    out <- c(
      out,
      list(
        hr(),
        downloadButton("dev_report", "Download the complete output as RTF"),
        hr()
      )
    )
  }
  out
}

tab_title_server <- function(input, output, session, tab) {
  if (global$dev == TRUE){
    tab$next_tab <- tab_name_to_number("type")
  }
}
