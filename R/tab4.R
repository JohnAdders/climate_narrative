tab4_ui <- function () {
  result = list(h2("Asset Manager Holdings"))
  result = c(result, exposure_grid(am_exposures,'asset|A'))
  return (result)
}