server <- function(input, output, session) {
  heartbeat(input, output, session)
  session$userData$verification_code <- substring(UUIDgenerate(), 1, 6)
  session$userData$captcha_validated <- FALSE

  if (file.exists("secret.yml")) {
    secret_pars <- read_yaml("secret.yml")
    session$userData$dev <- FALSE
    for (i in 1:length(secret_pars)) session$userData[[names(secret_pars)[i]]] <- secret_pars[[i]]
  } else {
    session$userData$dev <- TRUE
  }

  # the reactive variables (ultimately - the climate report)
  all_inputs <- reactive({
    x <- reactiveValuesToList(input)
    out <- data.frame(
      names = names(x),
      values = unlist(x, use.names = FALSE),
      stringsAsFactors = FALSE
    )
    new_col_names <- c("type", "subtype", "rowname", "product", "colname", "item", "product_description","product_text")
    out <- cbind(out, matrix(NA,nrow=nrow(out),ncol=length(new_col_names)))
    colnames(out) <- c("names", "values", new_col_names)
    splitted_names <- strsplit(out$names, "_", fixed = TRUE)
    for (i in 1:nrow(out)) {
      if (length(splitted_names[[i]]) == 6) {
        out[i, 3:8] <- splitted_names[[i]]
        if (is.null(products[[out$product[i]]])) {
          print(out$names[i])
          print(out[i,])
          warning(paste('No product description for', out$products[i]))
        } else {
          out$product_description[i] <- products[[out$product[i]]]$description
          out$product_text[i] <- products[[out$product[i]]]$text
        }
      } else if (length(splitted_names[[i]]) > 6) {
        warning(paste0("Unexpectedly large number of underscores in ", out$names[i]))
      }
    }
    out$materiality <- factor(out$values, levels = c("", "Low", "Medium", "High"), ordered = T)
    out
  })

  type_inputs <- reactive({
    out <- all_inputs()
    out <- out[(which(out$type == input$type & out$materiality != "")), ]
    return(out)
  })

  allow_report <- reactive({
    return(nrow(type_inputs())>0)
  })
  
  aggregated_type_inputs <- reactive({
    if (allow_report()) {
      aggregated_inputs <- aggregate(materiality ~ item, FUN = max, data = type_inputs())
      aggregated_inputs[order(aggregated_inputs$materiality, decreasing = TRUE), ]
    } else {
      return(data.frame(item = c(), materiality = c()))
    }
  })

  report_contents <- reactive({
    out <- "% Climate report\n\n"
    for (scenario in scenarios) {
      out <- c(
        out,
        get_scenario_descriptions(
          aggregated_type_inputs(),
          type_inputs(),
          scenario
        )
      )
    }
    out <- c(out, get_references(aggregated_type_inputs(), type_inputs()))
    out
  })

  temp_full_report <- reactive({
    # writing a full report to (temporary) file first
    # this is necessary as markdown::render takes file as an argument
    # not used at the moment, but do not delete - will be sent by email probably
    if (!exists("temp_md")) temp_md <- tempfile(fileext = ".md")
    file_conn <- file(temp_md)
    writeLines(report_contents(), file_conn)
    close(file_conn)
    temp_md
  })

  temp_scenario_report <- function(report_selection) {
    # writing a report to (temporary) file first
    # this is necessary as markdown::render takes file as an argument
    if (!exists("temp_scenario_md")) temp_scenario_md <- tempfile(fileext = ".md")
    file_conn <- file(temp_scenario_md)
    scenario_no <- which(sapply(scenarios, `[[`, i = "name") == report_selection)
    writeLines(
      report_contents()[c(1 + scenario_no, length(report_contents()))],
      file_conn
    )
    close(file_conn)
    temp_scenario_md
  }

  output$html_report <- renderUI({
    if (input$report_selection == "") {
      return(p("Please select a scenario"))
    }
    temp_html <- tempfile(fileext = ".html")
    result <- includeHTML(rmarkdown::render(
      input = temp_scenario_report(input$report_selection),
      output_file = temp_html,
      output_format = html_document(
        toc = TRUE,
        toc_depth = 2,
        number_sections = FALSE,
        self_contained = FALSE,
        fig_caption = FALSE
      )
    ))
    return(result)
  })

  # download button inspired by: https://shiny.rstudio.com/articles/generating-reports.html
  output$report <- downloadHandler(
    filename = "Climate Report.rtf",
    content = function(file, res_path = paste0(getwd(), "/www")) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      fs <- file.size(temp_scenario_report(input$report_selection))
      rmarkdown::render(
        input = temp_scenario_report(input$report_selection),
        output_file = file,
        output_format = rtf_document(
          toc = TRUE,
          toc_depth = 2,
          number_sections = FALSE,
          pandoc_args = c(
            paste0("--resource-path=", res_path),
            "--self-contained"
          )
        )
      )
      # I found that in some cases the rendering silently overwrites the markdown file
      # Cause unknown, maybe due to some weird blank characters instead of space?
      # Therefore added a control to throw error if the file is truncated in the process
      if (file.size(temp_scenario_report(input$report_selection)) != fs) stop("Rtf rendering issue - md file invisibly truncated!")
      removeModal()
    }
  )

  # finally, tab-specific server collation
  report_tab_no <- as.integer(factor('report', levels=ordered_tabs))
  always_true <- function() TRUE
  for (tab in tabs) {
    if (length(tab$next_tab)){
      if (tab$next_tab != report_tab_no){
        tab$server(input, output, session, switch_page, always_true)
      } else {
        tab$server(input, output, session, switch_page, allow_report)
      }
    } else {
      tab$server(input, output, session, switch_page, NULL)
    }
  }
}
