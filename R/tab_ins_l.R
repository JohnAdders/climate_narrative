tab_ins_l_server <- function(input, output, session, tab){
  exposure_grid_server(
    input,
    output,
    exposures$insurance_liabilities,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow=nrow(exposures$insurance_liabilities),
      ncol=ncol(exposures$insurance_liabilities)-2
    ),
    "insurance|L"
  )
}

tab_ins_l_ui <- function () {
  list(
    h2("Insurance Liability Exposures"),
    exposure_grid_ui("insurance|L")
  )
}

tab_ins_l_foot <- function(){
  list(
    helpText('A longer explanatory text regarding insurance liability exposures may be put here')
  )
}