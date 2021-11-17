server <- function(input, output, session) {
  heartbeat(input, output, session)
  session$userData$verification_code <- UUIDgenerate()
  session$userData$captcha_validated <- FALSE
  
  if (file.exists('secret.yml')){
    secret_pars <- read_yaml('secret.yml')
    for(i in 1:length(secret_pars)) session$userData[[names(secret_pars)[i]]] <- secret_pars[[i]]
  } else {
    session$userData$dev = TRUE
  }

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
    temp <- strsplit(out$names, "_", fixed=TRUE)
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
      } else if(length(temp[[i]]) > 6){
        warning(paste0('unexpectedly large number of underscores in ',out$names[i]))
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
      warning('All exposures are blank. Rendering the report contating scenario descriptions only')
      return(data.frame(item=c(),materiality=c()))
    }
  })

  report_contents <- reactive({
    out <- '% Climate report\n\n'
    for (scenario in scenarios){
      out <- c(
        out, 
        get_scenario_descriptions(
          aggregated_type_inputs(),
          type_inputs(),
          scenario
        )
      )
    }
    out
  })

  temp_report <- reactive({
    # writing a report to (temporary) file first
    # this is necessary as markdown::render takes file as an argument
    if(!exists('temp_md')) temp_md <- tempfile(fileext='.md')
    file_conn <- file(temp_md)
    writeLines(report_contents() , file_conn)
    close(file_conn)
    temp_md
  })

  temp_subset_report <- reactive({
    # writing a report to (temporary) file first
    # this is necessary as markdown::render takes file as an argument
    if(!exists('temp_subset_md')) temp_subset_md <- tempfile(fileext='.md')
    file_conn <- file(temp_subset_md)
    writeLines(
      report_contents()[1 + which(sapply(scenarios, `[[`, i='name')==input$report_selection)],
      file_conn
    )
    close(file_conn)
    temp_subset_md
  })

  output$rendered_report <- renderUI({
    # previous version, not supporting footnotes:
    # HTML(markdown::markdownToHTML(text=report_contents(), fragment.only=T))
    # or alternatively in a simpler way:
    # includeMarkdown(temp_report())
    if(input$report_selection=='') return(p('Please select a scenario'))
    temp_html <- tempfile(fileext='.html')
    includeHTML(rmarkdown::render(
      input=temp_subset_report(),
      output_file=temp_html,
      output_format=html_document(
        toc=TRUE,
        number_sections=FALSE,
        self_contained=FALSE,
        fig_caption=FALSE
      )
    ))
  })

  # download button inspired by: https://shiny.rstudio.com/articles/generating-reports.html
  output$report <- downloadHandler(
    filename = "Climate Report.rtf", # file extension defines the rendering process
    content = function(file, res_path=paste0(getwd(),'/www')) {
      showModal(
        modalDialog(
          'Report rendering in progress... when complete your download will start automatically',
          title='Climate Report',
          footer=NULL
        )
      )
      fs <- file.size(temp_report())
      rmarkdown::render(
        input=temp_report(),
        output_file=file,
        output_format=rtf_document(
          toc=TRUE,
          #fig_caption=FALSE,
          number_sections=FALSE,
          pandoc_args=c(
            paste0('--resource-path=', res_path),
            '--self-contained'
          )
        )
      )
      # I found that in some cases the rendering silently overwrites the markdown file
      # Cause unknown, maybe due to some weird blank characters instead of space?
      # Therefore added a control to throw error if the file is truncated in the process
      if(file.size(temp_report()) != fs) stop('Rtf rendering issue - md file invisibly truncated!')
      removeModal()
    }
  )
}
