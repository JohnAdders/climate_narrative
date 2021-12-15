server <- function(input, output, session) {
  heartbeat(input, output, session)
  session$userData$verification_code <- substring(uuid::UUIDgenerate(), 1, 6)
  session$userData$captcha_validated <- FALSE

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
        if (is.null(global$products[[out$product[i]]])) {
          print(out[i,])
          warning(paste('No product description for', out$product[i]))
        } else {
          out$product_description[i] <- global$products[[out$product[i]]]$description
          out$product_text[i] <- global$products[[out$product[i]]]$text
        }
      } else if (length(splitted_names[[i]]) > 6) {
        warning(paste0("Unexpectedly large number of underscores in ", out$names[i]))
      }
    }
    out$materiality <- factor(out$values, levels = c("", "Low", "Medium", "High"), ordered = T)
    out$materiality_num <- (as.integer(out$materiality) - 1)^2 + (as.integer(out$materiality) > 2)
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
      aggregated_inputs_factor <- aggregate(materiality ~ item, FUN = max, data = type_inputs())
      aggregated_inputs_numeric <- aggregate(
        materiality_num ~ item,
        FUN = function(x) cut(
          sum(x), 
          breaks = c(0, 4.5, 9.5, 100),
          labels = c("Low", "Medium", "High")
        ),
        data = type_inputs()
      )
      aggregated_inputs <- merge(aggregated_inputs_factor, aggregated_inputs_numeric)
      aggregated_inputs[order(aggregated_inputs$materiality, aggregated_inputs$materiality_num, decreasing = TRUE), ]
    } else {
      return(data.frame(item = c(), materiality = c()))
    }
  })

  report_contents <- reactive({
    out <- paste0(
      "---\n",
      "title: |\n",
      "  Climate report\n\n",
      "  ![](title.png)\n\n",
      "  ```{=rtf}\n",
      "  \\page\n",
      "  ```\n\n",
      "---\n\n"
    )
    for (scenario in global$scenarios) {
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

  # The functions below are writing a report to (temporary) file first
  # this is necessary as markdown::render takes file as an argument
  # there are 3 versions of the report, in each separate temp file:
  #    full report for email RTF,
  #    single scenario for HTML
  #    single scenario with common sectors (e.g. introduction) for button RTF

  temp_report_full <- reactive({
    # writing a full report to (temporary) file first
    # this is necessary as markdown::render takes file as an argument
    # not used at the moment, but do not delete - will be sent by email probably
    if (!exists("temp_md_full")) temp_md_full <- tempfile(fileext = ".md")
    file_conn <- file(temp_md_full)
    writeLines(report_contents(), file_conn)
    close(file_conn)
    temp_md_full
  })

  temp_report_scenario <- function(report_selection) {
    if (!exists("temp_md_scenario")) temp_md_scenario <- tempfile(fileext = ".md")
    file_conn <- file(temp_md_scenario)
    scenario_no <- c(
      which(sapply(global$scenarios, `[[`, i = "name") == report_selection),
      length(report_contents()) - 1
    )
    tempfun=function(x) gsub("\\(([[:graph:]]*)(.png)", paste0("(", getwd(),"/www/", "\\1\\2"), x,perl=T)
    #xmpl="![NGFS scenarios Framework](NGFS_scenarios_Framework_Orderly.png)"
    #print(xmpl)
    #print(tempfun(xmpl))
    writeLines(
      # plus one is for the title, not included in 'scenarios' but included in 'report_contents'
      tempfun(report_contents()[c(1 + scenario_no)]),
      # gsub(
      #   "\\(([[:graph:]]*)(.png)",
      #   paste0("(", getwd(), "/www", "\\\\1\\\\2"),
      #   report_contents()[c(1 + scenario_no)],
      #   perl=T
      # ),
      file_conn
    )
    close(file_conn)
    temp_md_scenario
  }

  temp_report_scenario_and_commons <- function(report_selection) {
    if (!exists("temp_md_scenario_and_commons")) temp_md_scenario_and_commons <- tempfile(fileext = ".md")
    file_conn <- file(temp_md_scenario_and_commons)
    scenario_no <- sort(
      c(
        which(sapply(global$scenarios, `[[`, i = "name") == report_selection),
        which(sapply(global$scenarios, function(sce) !sce$is_scenario)),
        length(report_contents()) - 1
      )
    )
    writeLines(
      # plus one is for the title, not included in 'scenarios' but included in 'report_contents'
      report_contents()[c(1, 1 + scenario_no)],
      file_conn
    )
    close(file_conn)
    temp_md_scenario_and_commons
  }

  output$html_report <- renderUI({
    if (input$report_selection == "") {
      return(p("Please select a scenario"))
    }
    temp_html <- tempfile(fileext = ".html")
    rmarkdown::render(
      input = temp_report_scenario(input$report_selection),
      output_file = temp_html,
      output_format = rmarkdown::html_document(
        number_sections = FALSE,
        self_contained = FALSE,
        fig_caption = FALSE
      )
    )
    # replace back the images links
    file_conn <- file(temp_html)
    temp <- readLines(file_conn)
    writeLines(
      gsub(
        paste0(getwd(), "/www"),
        "/climate_narrative",
        temp
      ),
      file_conn
    )
    close(file_conn)

    result <- includeHTML(temp_html)
    
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
      fs <- file.size(temp_report_scenario_and_commons(input$report_selection))
      rmarkdown::render(
        input = temp_report_scenario_and_commons(input$report_selection),
        output_file = file,
        output_format = rmarkdown::rtf_document(
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
      if (file.size(temp_report_scenario_and_commons(input$report_selection)) != fs) stop("Rtf rendering issue - md file invisibly truncated!")
      removeModal()
    }
  )

  # finally, tab-specific server function collation
  switch_page <- function(i) updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  report_tab_no <- as.integer(factor('report', levels=global$ordered_tabs))
  for (tab in global$tabs) {
    # "sum" below is a trick to include NULL case as sum(NULL)=0
    if (sum(tab$next_tab) == report_tab_no){
      tab$server(input, output, session, switch_page, allow_report)
    } else {
      tab$server(input, output, session, switch_page)
    }
  }
}
