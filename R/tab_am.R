tab_am_server <- function(input, output, session, tab) {
  exposure_grid_server(
    input,
    output,
    exposures$amexposures,
    matrix(
      "Placeholder for explanation, help text, remarks regarding this particular input field",
      nrow = nrow(exposures$amexposures),
      ncol = ncol(exposures$amexposures) - 2
    ),
    "asset_A",
    session$userData$dev
  )
}

tab_am_ui <- function() {
  list(
    h2("Asset manager Exposures"),
    exposure_grid_ui("asset_A")
  )
}

tab_am_foot <- function() {
  list(
    helpText("Enter your firm's exposures by asset class and sector")
  )
}