tab_am_ui <- function () {
  list(
    h2("Asset Manager Holdings"),
    exposure_grid(
      exposures$am_exposures,
      matrix("Placeholder for explanation, help text, remarks regarding this particular
         input field", nrow=nrow(exposures$am_exposures), ncol=ncol(exposures$am_exposures)-2),
      'asset|A'
    )
  )
}

tab_am_foot <- function() {
  list(
    helpText('A longer explanatory text regarding asset manager exposures may be put here')
  )
}