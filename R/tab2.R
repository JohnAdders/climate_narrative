tab2_ui <- function () {
  list(
    h2("Bank Exposures"),
    exposure_grid(exposures$bank_exposures, "bank|A")
  )
}

tab2_foot <- function(){
  list(
    helpText('A longer explanatory text may be put here')
  )
}