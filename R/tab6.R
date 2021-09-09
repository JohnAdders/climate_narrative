tab6_ui <- function () {
  list(
    # uiOutput('testOutput'), # uncomment for debugging
    # uiOutput("show_aggregated_inputs"), # uncomment for debugging
    uiOutput("renderedReport")
  )
}

tab6_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- switch(
        input$type,
        insurance = 5,
        asset = 4,
        2
      )
    }
  )
  
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
    out <- data.frame(
      names = names(x),
      values = unlist(x, use.names = FALSE),
      stringsAsFactors=FALSE
    )
    out$type <- rep(NA,nrow(out))
    out$subtype <- rep(NA,nrow(out))
    out$item <- rep(NA,nrow(out))
    out$rowname <- rep(NA,nrow(out))
    out$product <- rep(NA,nrow(out))
    out$colname <- rep(NA,nrow(out))
    out$product_description <- rep(NA,nrow(out))
    out$product_text <- rep(NA,nrow(out))
    temp <- strsplit(out$names, "|", fixed=TRUE)
    for(i in 1:nrow(out)){
      if(length(temp[[i]]) == 6){
        out$type[i] <- temp[[i]][1]
        out$subtype[i] <- temp[[i]][2]
        out$rowname[i] <- temp[[i]][3]
        out$product[i] <- temp[[i]][4]
        out$colname[i] <- temp[[i]][5]
        out$item[i] <- temp[[i]][6]
        out$product_description[i] <- products[[out$product[i]]]$description
        out$product_text[i] <- products[[out$product[i]]]$text
      }
    }
    out$materiality <- factor(out$values, levels=c('L','M','H'), ordered=T)
    out
  })
  
  # below are the functions and reactive expressions used to produce the report
  TypeInputs = reactive({
    out <- AllInputs()
    out <- out[(which(out$type==input$type)), ]
    return(out)
  })
  
  AggregatedTypeInputs <- reactive({
    temp <- TypeInputs()
    temp <- aggregate(materiality~item, FUN=max, data=temp)
    temp[order(temp$materiality, decreasing=TRUE), ]
  })
  
  get_exposure_description <- function(item, type_item_inputs){
    temp <- type_item_inputs[order(type_item_inputs$materiality), ]
    # conversion from factor back to string to ensure proper printing below
    temp$materiality <- as.character(temp$materiality)
    out <- paste0(
      '### ',
      exposure_classes[[item]][['name']],
      '\n\n',
      exposure_classes[[item]][['description']],
      '\n\nThe following rows contribute: \n\n',
      table_to_markdown(temp[,c('rowname','materiality','product','product_text','product_description')]),
      '\n\n'
    )
  }
  
  get_exposure_risk_description <- function(item, products, materiality, physical_or_transition, high_or_low){
    if(high_or_low == FALSE) return("")
    
    out <-paste0(
      "#### ",
      high_or_low,
      " ",
      physical_or_transition, 
      " risk\n\n",
      exposure_classes[[item]][[physical_or_transition]][[high_or_low]][['always']],
      '\n\n'
    )
    if(materiality == 'H') {
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
    for(i in 1:nrow(aggregated_table)){
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
  
  report_contents <- reactive({
    out <- '# Climate report\n\n'
    for(i in 1:length(scenarios)){
      out <- paste0(out, get_scenario_descriptions(
        AggregatedTypeInputs(),
        TypeInputs(),
        scenarios[[i]]$name,
        scenarios[[i]]$description,
        scenarios[[i]]$transition,
        scenarios[[i]]$physical
      ))
    }
    out
  })
  
  output$show_all_inputs <- renderTable({
    AllInputs()
  })
  
  output$show_aggregated_inputs <- renderTable({
    AggregatedTypeInputs()
  })
  
  output$renderedReport <- renderUI({
    HTML(markdown::markdownToHTML(text=report_contents() ))
  })
  output$testOutput <- renderUI({
    HTML(markdown::markdownToHTML(
      #text=table_to_markdown(data.frame(name=letters[1:3],value=1:3)) )
      text="test output\n\n# h1\n\n## h2\n\n### h3\n\n#### h4\n\n##### h5\n\nregular text\n"
    ))
  })
}
# the code below is currently not needed (the report is reactive).
# but I keep it, it may be more efficient to produce report only here
# (and not update it reactively if any input changes)
#observeEvent(
#  input$wizard,
#  if(input$wizard=='page_6') update_final_page(input, output, session)
#)