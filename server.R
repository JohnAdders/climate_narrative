server <- function(input, output, session) {
  # first, tab-specific server collation
  for (tab in tabs) {
    tab$server(input, output, session, switch_page)
  }

  # then the reactive variables (ultimately - the climate report)
  all_inputs <- reactive({
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
    out$materiality <- factor(out$values, levels=c('','Low','Medium','High'), ordered=T)
    out
  })
  
  type_inputs = reactive({
    out <- all_inputs()
    out <- out[(which(out$type == input$type & out$materiality != '')), ]
    return(out)
  })
  
  aggregated_type_inputs <- reactive({
    temp <- type_inputs()
    if(nrow(temp)){
      temp <- aggregate(materiality ~ item, FUN=max, data=temp)
      temp[order(temp$materiality, decreasing=TRUE), ]
    } else {
      # TODO decide what to show if all exposures are blank?
      warning('All exposures are blank')
      return(data.frame(item=c(),materiality=c()))
    }
  })

  report_contents <- reactive({
    out <- '# Climate report\n\n'
    for(i in 1:length(scenarios)){
      out <- paste0(out, get_scenario_descriptions(
        aggregated_type_inputs(),
        type_inputs(),
        scenarios[[i]]$name,
        scenarios[[i]]$description,
        scenarios[[i]]$transition,
        scenarios[[i]]$physical
      ))
    }
    out
  })

  # outputs definition
  output$show_all_inputs <- renderTable({
    all_inputs()
  })
  
  output$show_aggregated_inputs <- renderTable({
    aggregated_type_inputs()
  })
  
  output$rendered_report <- renderUI({
    HTML(markdown::markdownToHTML(text=report_contents(), fragment.only=T))
  })
  
  # download button inspired by: https://shiny.rstudio.com/articles/generating-reports.html
  output$report <- downloadHandler(
    filename = "Climate Report.rtf", # file extension defines the rendering process
    content = function(file) {
      # writing a report to (temporary) file first
      tempReport <- tempfile(fileext='.md')
      fileConn <- file(tempReport)
      writeLines(report_contents() , fileConn)
      close(fileConn)
      fs <- file.size(tempReport)
      rmarkdown::render(
        tempReport,
        output_file = file,
        envir = new.env(parent = globalenv())
      )
      # I found that in some cases the rendering silently overwrites the markdown file
      # Cause unknown, maybe due to some weird blank characters instead of space?
      # Therefore added a control to throw error if the file is truncated in the process
      if(file.size(tempReport) != fs) stop('Rtf rendering issue - md file invisibly truncated!')
    }
  )

  output$test_output <- renderUI({
    HTML(markdown::markdownToHTML(
      #text=table_to_markdown(data.frame(name=letters[1:3],value=1:3)) )
      text="test output\n\n# h1\n\n## h2\n\n### h3\n\n#### h4\n\n##### h5\n\nregular text\n"
    ))
  })
}