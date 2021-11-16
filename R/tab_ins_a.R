tab_ins_a_server <- function(input, output, session, tab){
  exposure_grid_server(
    input,
    output,
    exposures$insurance_assets,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow=nrow(exposures$insurance_assets),
      ncol=ncol(exposures$insurance_assets)-2
    ),
    "insurance_A",
    session$userData$dev
  )
}

tab_ins_a_ui <- function () {
  list(
    h2("Insurance Asset Exposures"),
    exposure_grid_ui("insurance_A")
  )
}

tab_ins_a_foot <- function() {
  list(
    helpText('A longer explanatory text regarding insurance asset exposures may be put here')
  )
}