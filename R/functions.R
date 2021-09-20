# helper function - a shortcut function to add element to the list
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
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
          inputId=paste(prefix, exposure_item, sep='|'),
          label='',
          choices=c('', 'Low', 'Medium', 'High'),
          selected='',
          # to allow empty string as a valid option I do not use selectize
          selectize=FALSE
    )))
  }
}

exposure_grid_row <- function(exposures_row, prefix, col_width) {
  items <- paste(
    names(exposures_row)[-(1:2)],
    exposures_row[-(1:2)],
    sep="|")
  items[exposures_row[-(1:2)] == ""] <- ""
  return (
    fluidRow(
      c(
        list(
          column(col_width, h5(exposures_row[1])),
          lapply(items, function(item) {
            exposure_grid_cell(item, paste(prefix,exposures_row[1], exposures_row[2], sep="|"), col_width)
            }
        )
      )
    )
  )
  )
}

exposure_grid <- function(exposures, label, col_width=floor(12/(ncol(exposures)-1))) {
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

# helper function to produce a markdown report
table_to_markdown <- function(table, additional_spaces=3, dot_to_space=TRUE){
  headers <- colnames(table)
  if(dot_to_space){
    headers <- gsub('.', '&nbsp;', headers, fixed=TRUE)
  }
  collapsor <- paste0(
    paste(
      rep("&nbsp;",additional_spaces),
      collapse=""),
      " | "
  )
  out <- paste(headers, collapse=collapsor)
  out <- paste0(
    out,
    '\n',
    paste(
      rep('---', ncol(table)),
      collapse=" | "
    ),
    '\n'
  )
  if(nrow(table)) for(i in 1:nrow(table)){
    out <- paste0(
      out, 
      paste(table[i,], collapse=collapsor),
      "\n"
    )
  }
  out <- paste0(out, '\n\n')
  return(out)
}

get_exposure_description <- function(item, type_item_inputs){
    temp <- type_item_inputs[order(type_item_inputs$materiality), ]
    # conversion from factor back to string to ensure proper printing below
    temp$materiality <- as.character(temp$materiality)
    temp2 <- temp[,c('rowname','materiality')]
    colnames(temp2) <- c('Exposure.row','Materiality')
    temp3 <- aggregate(
      temp2,
      by=list(
        Product.description=temp$product_description,
        Product.text=temp$product_text
      ),
      FUN=function(texts) paste(
        gsub(" ", "&nbsp;", texts),
        collapse='<br />'
      )
    )
    out <- paste0(
      '### ',
      exposure_classes[[item]][['name']],
      '\n\n',
      exposure_classes[[item]][['description']],
      '\n\nThe following rows contribute: \n\n',
      table_to_markdown(temp3),
      '\n\n'
    )
  }
  
  get_exposure_risk_description <- function(item, products, materiality, physical_or_transition, high_or_low){
    if(high_or_low == FALSE) return("")
    
    out <-paste0(
      "#### ",
      paste0(toupper(substring(physical_or_transition, 1, 1)), substring(physical_or_transition, 2)), 
      " risk (intensity: ",
      high_or_low,
      ")\n\n",
      exposure_classes[[item]][[physical_or_transition]][[high_or_low]][['always']],
      '\n\n'
    )
    if(materiality == 'High') {
      out <- paste0(
        out,
        exposure_classes[[item]][[physical_or_transition]][[high_or_low]][['high_materiality']],
        '\n\n'
      )
    }
    for(product in products){
      out <- paste0(
        out,
        exposure_classes[[item]][[physical_or_transition]][[high_or_low]][[product]],
        '\n\n'
      )
    }
    return(out)
  }
  
  get_scenario_descriptions <- function(aggregated_table, type_inputs, name, description, transition, physical){
    out <- paste0(
      '## ',
      name,
      '\n\n',
      description,
      '\n\n'
    )
    if(nrow(aggregated_table)) for (i in 1:nrow(aggregated_table)){
      item <- aggregated_table$item[i]
      materiality <- aggregated_table$materiality[i]
      type_item_inputs <- type_inputs[type_inputs$item == item,] 
      products <- unique(type_item_inputs$product)
      out <- paste0(
        out, 
        get_exposure_description(item, type_item_inputs),
        get_exposure_risk_description(item, products, materiality, "physical", physical),
        get_exposure_risk_description(item, products, materiality, "transition", transition)
      )
    }
    return(out)
  }