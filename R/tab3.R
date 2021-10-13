tab3_ui <- function () {
  list(
    h2("Insurance Asset Exposures"),
    exposure_grid(
      exposures$insurance_assets,
      matrix("Placeholder for explanation, help text, remarks regarding this particular
         input field", nrow=nrow(exposures$insurance_assets), ncol=ncol(exposures$insurance_assets)-2),
      "insurance|A")
  )
}

tab3_foot <- function() {
  list(
    helpText('A longer explanatory text regarding insurance asset exposures may be put here')
  )
}