tab3_ui <- function () {
  result = list(h2("Insurance Asset Exposures"))
  result = c(result, exposure_grid(insurance_assets))
  return (result)
}