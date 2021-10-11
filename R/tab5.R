tab5_ui <- function () {
  list(
    h2("Insurance Liabilities"),
    exposure_grid(
      exposures$insurance_liabilities,
      matrix("Placeholder for explanation, help text, remarks regarding this particular
         input field", nrow=nrow(exposures$insurance_liabilities), ncol=ncol(exposures$insurance_liabilities)-2),
      'insurance|L')
  )
}

tab5_foot <- function(){
  list(
    helpText('A longer explanatory text regarding insurance liability exposures may be put here')
  )
}