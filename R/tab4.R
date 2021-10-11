tab4_ui <- function () {
  list(
    h2("Asset Manager Holdings"),
    exposure_grid(exposures$am_exposures, 'asset|A')
  )
}

tab4_foot <- function() {
  list(
    helpText('A longer explanatory text may be put here')
  )
}