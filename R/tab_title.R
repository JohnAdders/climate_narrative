tab_title_ui <- function() {
  out <- list(
    img(src = "climate_narrative/title.png", alt = "CFRF logo4", width = "70%")
  )
  if (global$dev){
    out <- c(
      out,
      list(
        hr(),
        h3("Development version report download"),
        downloadButton("dev_report", "Download the complete output as RTF"),
        downloadButton("dev_report_2", "Download the sector by sector test RTF report"),
        hr()
      )
    )
  }
  out
}

tab_title_server <- function(input, output, session, tab) {
  if (global$dev == TRUE){
    tab$next_tab <- tab_name_to_number("intro")
  }
}
