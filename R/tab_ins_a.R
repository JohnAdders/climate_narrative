tab_ins_a_server <- function(input, output, session, tab) {
  exposure_grid_server(
    input,
    output,
    exposures$insuranceassets,
    produce_tooltip_matrix(exposures$insuranceassets),
    "insurance_A",
    session$userData$dev,
    width='6em'
  )
}

tab_ins_a_ui <- function() {
  list(
    h2("Insurance Asset Exposures"),
    exposure_grid_ui("insurance_A")
  )
}

tab_ins_a_foot <- function() {
  list(
    helpText("Enter your firm's asset exposures by asset class and sector")
  )
}
