tab_am_server <- function(input, output, session, tab){
  exposure_grid_server(
    input,
    output,
    exposures$am_exposures,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow=nrow(exposures$am_exposures),
      ncol=ncol(exposures$am_exposures)-2
    ),
    "asset|A"
  )
}

tab_am_ui <- function () {
  list(
    h2("Asset manager Exposures"),
    exposure_grid_ui("asset|A")
  )
}

tab_am_foot <- function() {
  list(
    helpText('A longer explanatory text regarding asset manager exposures may be put here')
  )
}