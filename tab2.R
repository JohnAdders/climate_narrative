tab2_ui <- function () {
  result = list(h2("Bank Exposures"))
  result = c(result, exposure_grid(bank_exposures,"bank_A"))
  return (result)
}