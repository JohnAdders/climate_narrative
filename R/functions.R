# helper function - a shortcut function to add element to the list
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

# helper function to read all yaml files as a named R list
read_yaml_dir <- function(directory){
  list <- lapply(dir(path=directory),function(file) read_yaml(paste0(directory, '/', file)))
  names(list) <- sapply(dir(path=directory), function(file) strsplit(file, '.', fixed=T)[[1]][1])
  return(list)
}

# helper function to creat a markdown table from r object
table_to_markdown <- function(table, additional_spaces=3){
  collapsor <- paste0(paste(rep("&nbsp;",additional_spaces),collapse=" "), " | ")
  out <- paste(colnames(table), collapse=collapsor)
  out <- paste0(out, '\n', paste(rep('---', ncol(table)), collapse=" | "), '\n')
  for(i in 1:nrow(table)){
    out <- paste0(out, paste(table[i,], collapse=collapsor), "\n")
  }
  out <- paste0(out, '\n\n')
  return(out)
}

# helper functions to produce the layout of tabs (cell, row, whole table)
exposure_grid_cell <- function(exposure_item, prefix, col_width) {
  if (exposure_item == "") {
    return (column(
      col_width,
      p("")
    ))
  } else {
    return(
      column(
        col_width,
        selectInput(
          paste(prefix,exposure_item,sep='|'),
          '',
          list('L', 'M', 'H')
    )))
  }
}

exposure_grid_row <- function(exposures_row, prefix, col_width) {
  items = paste(names(exposures_row)[-(1:2)], exposures_row[-(1:2)], sep="|")
  items[exposures_row[-(1:2)] == ""] <- ""
  return (
    fluidRow(
      c(
        list(
          column(col_width, p(exposures_row[1])),
          lapply(items, function(item) {
            exposure_grid_cell(item, paste(prefix,exposures_row[1], exposures_row[2], sep="|"), col_width)
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
          # do not show 'product' column
          colnames(exposures)[-2],
          # when reading csv's R by default substitutes spaces with dots in the headers, here we reverse this for a nicer output
          function(header) column(col_width, h4(gsub(".", " ", header, fixed=TRUE))) 
        )
      )
    ),
    apply(exposures, 1, exposure_grid_row, label, col_width=col_width)
  )
  return (rows)
}