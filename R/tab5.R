tab5_ui <- function () {
  list(
    h2("Insurance Liabilities"),
    exposure_grid(exposures$insurance_liabilities, 'insurance|L')
  )
}

tab5_foot <- function(){
  list(
    helpText('A longer explanatory text may be put here')
  )
}