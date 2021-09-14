tab2_ui <- function () {
  result <- list(h2("Bank Exposures"))
  result <- c(result, exposure_grid(exposures$bank_exposures, "bank|A"))
  return (result)
}