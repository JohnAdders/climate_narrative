tab5_ui <- function () {
  result <- list(
    h2("Insurance Liabilities"),
    exposure_grid(exposures$insurance_liabilities, 'insurance|L')
    helpText('A longer explanatory text may be put here')
  )
  return (result)
}