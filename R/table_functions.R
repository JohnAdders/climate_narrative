table_to_markdown <- function(table, additional_spaces=3){
  collapsor <- paste0(paste(rep("&nbsp;",additional_spaces),collapse=" "), " | ")
  out <- paste(colnames(table), collapse=collapsor)
  out <- paste0(out, '\n', paste(rep('---', ncol(table)), collapse=" | "), '\n')
  for(i in 1:nrow(table)){
    out <- paste0(out, paste(table[i,], collapse=collapsor), "\n")
  }
  out <- paste0(out, '\n\n')
  print(out)
  return(out)
}

exposure_grid_cell <- function(exposure_item, prefix, col_width) {
  if(exposure_item == "") {
    return (column(col_width, p("")))
  } else {
    return(column(col_width,selectInput(paste(prefix,exposure_item,sep='|'),'',list('L','M','H'))))
  }
}

exposure_grid_row <- function(exposures_row, prefix, col_width) {
  #items = exposures_row[-1]
  items = paste(names(exposures_row)[-(1:2)],exposures_row[-(1:2)],sep="|")
  items[exposures_row[-(1:2)]==""]=""
  return (
    fluidRow(
      c(
        list(
          column(col_width, p(exposures_row[1])),
          lapply(items, function(item) {
            exposure_grid_cell(item, paste(prefix,exposures_row[1],exposures_row[2],sep="|"),col_width)
            }
        )
      )
    )
  )
  )
}

exposure_grid <- function(exposures, label, col_width=2) {
  rows = c(
    list(
      fluidRow(
        lapply(
          colnames(exposures),
          # when reading csv's R by default substitutes spaces with dots in the headers, here we reverse this for a nicer output
          function(header) column(col_width, h4(gsub("."," ",header,fixed=TRUE))) 
        )
      )
    ),
    apply(exposures, 1, exposure_grid_row, label, col_width=col_width)
  )
  return (rows)
}