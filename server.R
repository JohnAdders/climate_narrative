server <- function(input, output, session) {
  for (tab in tabs) {
    tab$server(input, output, session, switch_page)
  }
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

  AggregatedTypeInputs <- reactive({
    temp <- AllInputs()
    temp <- temp[(temp$type==input$type),]
    temp$values <- factor(temp$values,levels=c('L','M','H'),ordered=T)
    temp <- aggregate(values~item,FUN=max,data=temp)
    temp[order(temp$values,decreasing=TRUE),]
  })

  # output$show_aggregated_inputs <- renderTable({
  #   AggregatedTypeInputs()
  # })

  output$renderedReport <- renderUI({           
	includeMarkdown(knitr::knit('markdown/report.Rmd'))

})
}