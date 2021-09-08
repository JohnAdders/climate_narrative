tab6_ui <- function () {
  list(
    #uiOutput("show_aggregated_inputs"), # uncomment for debugging
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
    out$colname <- rep(NA,nrow(out))
    temp <- strsplit(out$names,"_")
    for(i in 1:nrow(out)){
      if(length(temp[[i]])==5){
        out$type[i] <- temp[[i]][1]
        out$subtype[i] <- temp[[i]][2]
        out$rowname[i] <- temp[[i]][3]
        out$colname[i] <- temp[[i]][4]
        out$item[i] <- temp[[i]][5]
      }
    }
    out
  })

  # below are the functions and reactive expressions used to produce the report
  AggregatedTypeInputs <- reactive({
    temp <- AllInputs()
    temp <- temp[(temp$type==input$type),]
    temp$values <- factor(temp$values,levels=c('L','M','H'),ordered=T)
    temp <- aggregate(values~item,FUN=max,data=temp)
    temp[order(temp$values,decreasing=TRUE),]
  })

  get_exposure_description <- function(item){
    #out <- list()
    #out <- add_param(out, h3(exposure_classes[[item]][['name']]))
    #out <- add_param(out, p(exposure_classes[[item]][['description']]))
    out <- paste0(
      '### ',
      exposure_classes[[item]][['name']],
      '\n\n',
      exposure_classes[[item]][['description']],
      '\n\n'
    )
    # Your Exposures that gives this a table of name of row, expsoure 
    # and product descrpition text
  }

  get_exposure_risk_descriptions <- function(item, materiality, physical_or_transition, high_or_low){
    #out <- list()
    #for(i in 1:nrow(aggregated_table)){
      #out <- add_param(out, h3(item))#aggregated_table$item[i]))
      # out <- add_param(out, p(report_pieces[[physical_or_transition]][[high_or_low]][['always']][[item]]))#aggregated_table$item[i]]]))
      out <- paste0(report_pieces[[physical_or_transition]][[high_or_low]][['always']][[item]],'\n\n')
      #if(aggregated_table$values[i]=='H') {
      if(materiality=='H') {
        #out <- add_param(out, p(report_pieces[[physical_or_transition]][[high_or_low]][['extra']][[item]]))#[[aggregated_table$item[i]]]))
        out <- paste0(out, report_pieces[[physical_or_transition]][[high_or_low]][['extra']][[item]],'\n\n')#[[aggregated_table$item[i]]]))
      }
    #}
    return(out)
  }
  
  get_scenario_descriptions <- function(aggregated_table, name, description, transition, physical){
    #out <- list()
    #out <- add_param(out, h2(name))
    #out <- add_param(out, p(description))
    out <- paste0(
      '## ',
      name,
      '\n\n',
      description,
      '\n\n'
    )
    for(i in 1:nrow(aggregated_table)){
      #out <- add_param(out, h3(paste0("Exposure class: ",aggregated_table$item[i])))
      #out <- c(out, get_exposure_description(aggregated_table$item[i]))
      out <- paste0(out, get_exposure_description(aggregated_table$item[i]))
      if(physical != 'none') {
        #out <- add_param(out, h4(paste0(physical, " physical risk")))
        #out <- c(out, get_exposure_risk_descriptions(aggregated_table$item[i], aggregated_table$values[i],"physical", physical))
        out <- paste0(
          out, 
          "#### ",
          physical, 
          " physical risk\n\n")
      }
      if(transition != 'none') {
        out <- paste0(out, (paste0("#### ", transition, " transition risk")))
        out <- paste0(out, get_exposure_risk_descriptions(aggregated_table$item[i], aggregated_table$values[i], "transition", transition))
      }
    }
    return(out)
  }

  report_contents <- reactive({
    aggregated_table <- AggregatedTypeInputs()
    #out <- list(h1('Climate report'))
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
    print(markdown::markdownToHTML(text=out))
    out
    #out <- c(
    #  get_scenario_descriptions(aggregated_table,2.5),
    #  get_scenario_descriptions(aggregated_table,4)
    #)
  })
  
  output$show_aggregated_inputs <- renderTable({
     AggregatedTypeInputs()
  })
  
  output$renderedReport <- renderUI({
    HTML(markdown::markdownToHTML(text=report_contents() ))
  })
}
  # the code below is currently not needed (the report is reactive).
  # but I keep it, it may be more efficient to produce report only here
  # (and not update it reactively if any input changes)
  #observeEvent(
  #  input$wizard,
  #  if(input$wizard=='page_6') update_final_page(input, output, session)
  #)