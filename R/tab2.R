tab2_ui <- function () {
  result <- list(
    h2("Bank Exposures"),
    exposure_grid(exposures$bank_exposures, "bank|A"),
    helpText('A longer explanatory text may be put here')
  )
  return (result)
}