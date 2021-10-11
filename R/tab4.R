tab4_ui <- function () {
  result <- list(
    h2("Asset Manager Holdings"),
    exposure_grid(exposures$am_exposures, 'asset|A'),
    helpText('A longer explanatory text may be put here')
  )
  return (result)
}