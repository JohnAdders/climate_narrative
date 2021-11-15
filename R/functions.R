# helper function - a shortcut function to add element to the list
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

# helper function to get object by its name but return NULL (not error) if it does not exist
get_or_null = function(name) if(exists(name)) return(get(name)) else return(NULL)

# helper functions to produce the layout of tabs (cell, row, whole table)
exposure_grid_cell <- function(exposure_item, prefix, tooltip_text=NULL) {
  if (exposure_item == "") {
    #column(
    #  col_width,
      p("")
    #)
  } else {
    form <- selectInput(
        inputId=paste(prefix, exposure_item, sep='|'),
        label='',
        choices=c('', 'Low', 'Medium', 'High'),
        selected='',
        # to allow empty string as a valid option I do not use selectize
        selectize=FALSE
    )
    if(!is.null(tooltip_text)){
      form <- with_tippy(form, tooltip_text)
    }  
    form
  }
}

exposure_grid_ui <- function(label) {
  tableOutput(label)
}

exposure_grid_server <- function(
  input,
  output, 
  exposure_matrix,
  tooltip_matrix,
  label
) {
  layout <- matrix("", nrow=nrow(exposure_matrix),ncol=ncol(exposure_matrix)-1)
  colnames(layout) <- colnames(exposure_matrix)[-2] 
  for(i in 1:nrow(layout)){
    layout[i,1] <- as.character(div(exposure_matrix[i,1], class = "verticalcenter"))
    for(j in 2:ncol(layout)){
      layout[i,j] <- as.character(
        exposure_grid_cell(
          exposure_matrix[i,j+1],
          paste(label, exposure_matrix[i,1], exposure_matrix[i,2], colnames(exposure_matrix)[j+1], sep="|"),
          #col_width, 
          tooltip_matrix[i,j-1]
        )
      )
    }
  }  
  output[[label]] <- renderTable(
    layout,
    sanitize.text.function=function(x) x,
    sanitize.colnames.function=function(x) gsub('.', '&nbsp;', x, fixed=TRUE)
  ) 
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
      '## ',
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
      "### ",
      paste0(toupper(substring(high_or_low, 1, 1)), substring(high_or_low, 2)), 
      " ",
      physical_or_transition,
      " risk\n\n",
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
      '# ',
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
        get_exposure_risk_description(item, products, materiality, "transition", transition),
        get_exposure_risk_description(item, products, materiality, "physical", physical)
      )
    }
    return(out)
  }

# heartbeat function to prevent app closing due to inactivity
heartbeat = function(input, output, session) {
  beep <- reactiveTimer(55 * 1000)
  output[["__heartbeat"]] <- renderText({
    beep()
    " "
  })
}

heartbeat_footer = function() {
  list(
    hr(),
    tag('footer', list(
      img(src='aviva_logo.png', alt='Aviva logo', height=50),
      p("Developed in Aviva by Krzysztof Opalski, John Adcock")
    ))
  )
}

### captcha functions copied from https://github.com/sarthi2395/shinygCAPTCHAv3/blob/master/R/shinygCAPTCHAv3.R

GreCAPTCHAv3Ui <- function(siteKey) {
tagList(tags$head(
  tags$script(src = paste0("https://www.google.com/recaptcha/api.js?render=",siteKey)),
))
}

GreCAPTCHAv3js <- function(siteKey, action, fieldID) {
  runjs(paste0("
        grecaptcha.ready(function () {
          grecaptcha.execute('", siteKey, "', { action: '", action, "' }).then(function (token) {
			      Shiny.onInputChange('", fieldID, "',token);
      		});
	      });
      "))
}

GreCAPTCHAv3Server <- function(secretKey, reCaptchaResponse) {
  gResponse <- POST(
    "https://www.google.com/recaptcha/api/siteverify", body = list(
      secret = secretKey,
      response = reCaptchaResponse
    )
  )

  if(gResponse$status_code==200){
    return(fromJSON(content(gResponse, "text")))
  }
} 