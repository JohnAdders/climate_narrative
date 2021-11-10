tab_bank_server <- function(input, output, session, tab){
  exposure_grid_server(
    input,
    output,
    exposures$bank_exposures,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow=nrow(exposures$bank_exposures),
      ncol=ncol(exposures$bank_exposures)-2
    ),
    "bank|A"
  )
}

tab_bank_ui <- function () {
  list(
    h2("Bank Exposures"),
    exposure_grid_ui("bank|A")
  )
}

tab_bank_foot <- function(){
  list(
    helpText('A longer explanatory text regarding bank exposures may be put here')
  )
}