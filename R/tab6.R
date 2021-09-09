tab6_ui <- function () {
  list(
    uiOutput('testOutput'), # uncomment for debugging
    uiOutput("show_aggregated_inputs"), # uncomment for debugging
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
    temp <- strsplit(out$names, "|", fixed=TRUE)
    for(i in 1:nrow(out)){
      if(length(temp[[i]])==6){
        out$type[i] <- temp[[i]][1]
        out$subtype[i] <- temp[[i]][2]
        out$rowname[i] <- temp[[i]][3]
        out$product[i] <- temp[[i]][4]
        out$colname[i] <- temp[[i]][5]
        out$item[i] <- temp[[i]][6]
      }
    }
    out
  })

  # below are the functions and reactive expressions used to produce the report
  AggregatedTypeInputs <- reactive({
    temp <- AllInputs()
    temp <- temp[(temp$type==input$type),]
    temp$materiality <- factor(temp$values,levels=c('L','M','H'),ordered=T)
    temp <- aggregate(materiality~item,FUN=max,data=temp)
    temp[order(temp$materiality,decreasing=TRUE),]
  })

  get_exposure_description <- function(item){
    temp <- AllInputs()
    temp$materiality <- factor(temp$values,levels=c('L','M','H'),ordered=T)
    temp <- temp[!is.na(temp$type) & (temp$type==input$type) & (temp$item==item),c('rowname','product','materiality')]
    temp <- temp[order(temp$materiality), ]
    temp$materiality <- as.character(temp$materiality)
    # conversion from factor back to string to ensure proper printing below
    out <- paste0(
      '### ',
      exposure_classes[[item]][['name']],
      '\n\n',
      exposure_classes[[item]][['description']],
      '\n\nThe following rows contribute: \n\n',
      table_to_markdown(temp),
      '\n\n'
    )
  }

  get_exposure_risk_descriptions <- function(item, materiality, physical_or_transition, high_or_low){
    out <- paste0(report_pieces[[physical_or_transition]][[high_or_low]][['always']][[item]],'\n\n')
    if(materiality=='H') {
      out <- paste0(
        out,
        report_pieces[[physical_or_transition]][[high_or_low]][['extra']][[item]],
        '\n\n'
      )
    }
    return(out)
  }
  
  get_scenario_descriptions <- function(aggregated_table, name, description, transition, physical){
    temp <- AllInputs()
      temp$materiality <- factor(temp$values,levels=c('L','M','H'),ordered=T)
      temp <- temp[!is.na(temp$type) & (temp$type==input$type), c('rowname','product','materiality')]
      products <- unique(temp$product)
    out <- paste0(
      '## ',
      name,
      '\n\n',
      description,
      '\n\n'
    )
    for(i in 1:nrow(aggregated_table)){
      out <- paste0(out, get_exposure_description(aggregated_table$item[i]))
      if(physical != FALSE) {
        out <- paste0(
          out, 
          "#### ",
          physical, 
          " physical risk\n\n",
          get_exposure_risk_descriptions(aggregated_table$item[i], aggregated_table$materiality[i], "physical", physical)
        )
        for (product in products){
          product_specific_text <- report_pieces[['physical']][[physical]][[product]]
          if(!is.null(product_specific_text)){
            out <- paste0(out, product_specific_text, '\n\n')    
          }
        }
      }
      if(transition != FALSE) {
        out <- paste0(
          out, 
          "#### ", 
          transition,
          " transition risk\n\n",
          get_exposure_risk_descriptions(aggregated_table$item[i], aggregated_table$materiality[i], "transition", transition)
        )
        for (product in products){
          product_specific_text <- report_pieces[['transition']][[transition]][[product]]
          if(!is.null(product_specific_text)){
            out <- paste0(out, product_specific_text, '\n\n')    
          }
        }
      } 
      #out <- paste0(out, 'TODO: optional product specific text\n\n')
    }
    return(out)
  }

  report_contents <- reactive({
    aggregated_table <- AggregatedTypeInputs()
    out <- '# Climate report\n\n'
    for(i in 1:length(scenarios)){
      out <- paste0(out, get_scenario_descriptions(
        aggregated_table,
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