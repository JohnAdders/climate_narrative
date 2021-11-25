tab_ins_a_server <- function(input, output, session, tab){
  exposure_grid_server(
    input,
    output,
    exposures$insuranceassets,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow=nrow(exposures$insuranceassets),
      ncol=ncol(exposures$insuranceassets)-2
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
    helpText("Enter your firm's asset exposures by asset class and sector")
  )
}