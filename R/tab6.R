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

  get_risk_descriptions <- function(aggregated_table, physical_or_transition, high_or_low){
    out <- list()
    for(i in 1:nrow(aggregated_table)){
      out <- add_param(out, h3(aggregated_table$item[i]))
      out <- add_param(out, p(report_pieces[[physical_or_transition]][[high_or_low]][['always']][[aggregated_table$item[i]]]))
      if(aggregated_table$values[i]=='H') {
        out <- add_param(out, p(report_pieces[[physical_or_transition]][[high_or_low]][['extra']][[aggregated_table$item[i]]]))
      }
    }
    return(out)
  }
  
  get_scenario_descriptions <- function(aggregated_table, scenario_degree){
    out <- list()
    if(scenario_degree==2.5){
      out <- add_param(out, h1("Scenario 2.5 degree"))
      out <- add_param(out, h2("low physical risk"))
      out <- c(out, get_risk_descriptions(aggregated_table, "physical", "low"))
      out <- add_param(out, h2("high transition risk"))
      out <- c(out, get_risk_descriptions(aggregated_table, "transition", "high"))
    } else if (scenario_degree==4){
      out <- add_param(out, h1("Scenario 4 degree"))
      out <- add_param(out, h2("high physical risk"))
      out <- c(out, get_risk_descriptions(aggregated_table, "physical", "high"))
    } else {
      out <- list(h1('scenario not recognised!'))
    }
    return(out)
  }

  report_contents <- reactive({
    aggregated_table <- AggregatedTypeInputs()
    out <- c(
      get_scenario_descriptions(aggregated_table,2.5),
      get_scenario_descriptions(aggregated_table,4)
    )
  })
  
  output$show_aggregated_inputs <- renderTable({
     AggregatedTypeInputs()
  })
  
  output$renderedReport <- renderUI({
    report_contents() 
  })
}
  # the code below is currently not needed (the report is reactive).
  # but I keep it, it may be more efficient to produce report only here
  # (and not update it reactively if any input changes)
  #observeEvent(
  #  input$wizard,
  #  if(input$wizard=='page_6') update_final_page(input, output, session)
  #)