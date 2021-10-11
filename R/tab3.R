tab3_ui <- function () {
  list(
    h2("Insurance Asset Exposures"),
    exposure_grid(exposures$insurance_assets, "insurance|A")
  )
}

tab3_foot <- function() {
  list(
    helpText('A longer explanatory text may be put here')
  )
}