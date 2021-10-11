tab3_ui <- function () {
  result <- list(
    h2("Insurance Asset Exposures"),
    exposure_grid(exposures$insurance_assets, "insurance|A"),
    helpText('A longer explanatory text may be put here')
  )
  return (result)
}