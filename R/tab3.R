tab3_ui <- function () {
  result <- list(h2("Insurance Asset Exposures"))
  result <- c(result, exposure_grid(exposures$insurance_assets, "insurance|A"))
  return (result)
}