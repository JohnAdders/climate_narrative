exposure_grid_cell <- function(exposure_item, row_name, col_width) {
  if(exposure_item == "") {
    return (column(col_width, p("")))
  } else {
    #return (column(1, p("H/M/L")))
    return(column(col_width,selectInput(paste(exposure_item, row_name,sep='_'),'',list('H','M','L'))))
  }
}

exposure_grid_row <- function(exposures_row,col_width) {
  return (
    fluidRow(
      c(
        list(
          column(col_width, p(exposures_row[1])),
          #lapply(exposures_row[-1], function(item) {exposure_grid_cell(item, p(exposures_row[1]))})
          lapply(exposures_row[-1], function(item) {exposure_grid_cell(item, exposures_row[1],col_width)})
        )
      )
    )
  )
}

exposure_grid <- function(exposures,col_width=2) {
  rows = c(
    list(
      fluidRow(
        lapply(
          colnames(exposures),
          function(header) {
            print(header)
            print(gsub("."," ",header,fixed=TRUE))
            column(col_width, h4(gsub("."," ",header,fixed=TRUE))) # when reading csv's R by default substitutes spaces with dots in the headers, here we reverse this for a nicer output
          }
        )
      )
    ),
    apply(exposures, 1, exposure_grid_row,col_width=col_width)
  )
  return (rows)
}