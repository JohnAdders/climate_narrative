tab_ins_l_server <- function(input, output, session, tab) {
  exposure_grid_server(
    input,
    output,
    exposures$insuranceliabilities,
    produce_tooltip_matrix(exposures$insuranceliabilities),
    "insurance_L",
    session$userData$dev,
    width='10em'
  )
}

tab_ins_l_ui <- function() {
  list(
    h2("Insurance Liability Exposures"),
    exposure_grid_ui("insurance_L")
  )
}

tab_ins_l_foot <- function() {
  list(
    helpText("Enter your firm's liability exposures by type")
  )
}
