tab5_ui <- function () {
  result <- list(h2("Insurance Liabilities"))
  result <- c(result, exposure_grid(exposures$insurance_liabilities, 'insurance|L'))
  return (result)
}