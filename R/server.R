#' Main Shiny server function
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @importFrom stats aggregate
#' @export
#'
server <- function(input, output, session) {
  heartbeat(input, output, session)
  session$userData$verification_code <- substring(uuid::UUIDgenerate(), 1, 6)
  session$userData$captcha_validated <- FALSE
  session$userData$temp_md_full <- tempfile(fileext = ".md")
  session$userData$temp_md_scenario <- tempfile(fileext = ".md")
  session$userData$temp_md_scenario_and_commons <- tempfile(fileext = ".md")
  session$userData$temp_rtf <- tempfile(fileext = ".rtf")
  session$userData$temp_md_dev <- tempfile(fileext = ".md")
  session$userData$temp_rtf_dev <- tempfile(fileext = ".rtf")

  # the reactive variables (ultimately - the climate report)
  all_inputs <- reactive({
    x <- reactiveValuesToList(input)
    out <- data.frame(
      names = names(x),
      values = unlist(x, use.names = FALSE),
      stringsAsFactors = FALSE
    )
    new_col_names <- c("type", "subtype", "rowname", "product", "colname", "item", "product_description", "product_text")
    out <- cbind(out, matrix(NA, nrow = nrow(out), ncol = length(new_col_names)))
    colnames(out) <- c("names", "values", new_col_names)
    splitted_names <- strsplit(out$names, "_", fixed = TRUE)
    for (i in 1:nrow(out)) {
      if (length(splitted_names[[i]]) == 6) {
        out[i, 3:8] <- splitted_names[[i]]
        if (is.null(global$products[[out$product[i]]])) {
          print(out[i, ])
          warning(paste("No product description for", out$product[i]))
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
    return(nrow(type_inputs()) > 0)
  })

  aggregated_type_inputs <- reactive({
    if (allow_report()) {
      aggregated_inputs_factor <- stats::aggregate(materiality ~ item, FUN = max, data = type_inputs())
      aggregated_inputs_numeric <- stats::aggregate(
        materiality_num ~ item,
        FUN = function(x) {
          cut(
            sum(x),
            breaks = c(0, 4.5, 9.5, 100),
            labels = c("Low", "Medium", "High")
          )
        },
        data = type_inputs()
      )
      aggregated_inputs <- merge(aggregated_inputs_factor, aggregated_inputs_numeric)
      aggregated_inputs[order(aggregated_inputs$materiality, aggregated_inputs$materiality_num, decreasing = TRUE), ]
    } else {
      return(data.frame(item = c(), materiality = c()))
    }
  })
  
  aggregated_all_inputs <- reactive({
    if (allow_report()) {
      aggregated_inputs_factor <- stats::aggregate(materiality ~ item, FUN = max, data = all_inputs())
      aggregated_inputs_numeric <- stats::aggregate(
        materiality_num ~ item,
        FUN = function(x) {
          cut(
            sum(x),
            breaks = c(0, 4.5, 9.5, 100),
            labels = c("Low", "Medium", "High")
          )
        },
        data = all_inputs()
      )
      aggregated_inputs <- merge(aggregated_inputs_factor, aggregated_inputs_numeric)
      aggregated_inputs[order(aggregated_inputs$materiality, aggregated_inputs$materiality_num, decreasing = TRUE), ]
    } else {
      return(data.frame(item = c(), materiality = c()))
    }
  })

  aggregated_type_inputs_subset <- reactive({
    if (input$report_sector_selection == ""){
      return (aggregated_type_inputs())
    } else {
      out <- aggregated_type_inputs()
      selected_item <- names(
        which(sapply(global$exposure_classes, `[[`, i = "name") == input$report_sector_selection)
      )
      return(out[out$item == selected_item, ])
    }
  })
  
  # update the available sectors, only after tab switch
  observeEvent(
    input$wizard,
    {
      updateSelectInput(
        session,
        "report_sector_selection",
        choices=c(
          "",
          unname(sapply(global$exposure_classes, `[[`, i = "name"))[
            names(global$exposure_classes) %in% aggregated_type_inputs()$item
          ]
        )
      )
    }
  )
  

  #report_contents <- reactive({
  get_report_contents <- function(aggregated_inputs, inputs){
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
          aggregated_inputs,
          inputs,
          scenario
        )
      )
    }
    out <- c(out, get_references(aggregated_inputs, inputs))
    out
  }

  output$html_report <- renderUI({
    if (input$report_scenario_selection == "" & input$report_sector_selection == "") {
      return(p("Please select a scenario or a sector"))
    }
    temp_html <- tempfile(fileext = ".html")
    produce_selective_report(
      get_report_contents(aggregated_type_inputs_subset(), type_inputs()),
      input$report_scenario_selection,
      FALSE,
      session$userData$temp_md_scenario
    )
    rmarkdown::render(
      input = session$userData$temp_md_scenario,
      output_file = temp_html,
      output_format = rmarkdown::html_document(
        toc = TRUE,
        toc_depth = 2,
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
        system.file("www", package = "climate.narrative"),
        "climate_narrative",
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
    content = function(file, res_path = system.file("www", package = "climate.narrative")) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      produce_selective_report(
        get_report_contents(aggregated_type_inputs_subset(), type_inputs()),
        input$report_scenario_selection,
        TRUE,
        session$userData$temp_md_scenario_and_commons
      )
      fs <- file.size(session$userData$temp_md_scenario_and_commons)
      rmarkdown::render(
        input = session$userData$temp_md_scenario_and_commons,
        output_file = session$userData$temp_rtf,
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
      if (file.size(session$userData$temp_md_scenario_and_commons) != fs) stop("Rtf rendering issue - md file invisibly truncated!")
      # by default the table of contents in pandoc output does not work, fixing it manually
      rtf_fix_table_of_contents(session$userData$temp_rtf)
      rtf_center_images(session$userData$temp_rtf)
      removeModal()
      file.copy(session$userData$temp_rtf, file)
    }
  )

  output$dev_report <- downloadHandler(
    filename = "All_Outputs.rtf",
    content = function(file, res_path = system.file("www", package = "climate.narrative")) {
      print(all_inputs())
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      produce_full_report(
        get_report_contents(aggregated_all_inputs(), all_inputs()),
        session$userData$temp_md_dev
      )
      fs <- file.size(session$userData$temp_md_dev)
      rmarkdown::render(
        input = session$userData$temp_md_dev,
        output_file = session$userData$temp_rtf_dev,
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
      if (file.size(session$userData$temp_md_dev) != fs) stop("Rtf rendering issue - md file invisibly truncated!")
      # by default the table of contents in pandoc output does not work, fixing it manually
      rtf_fix_table_of_contents(session$userData$temp_rtf_dev)
      rtf_center_images(session$userData$temp_rtf_dev)
      removeModal()
      file.copy(session$userData$temp_rtf_dev, file)
    }
  )

  # finally, tab-specific server function collation
  switch_page <- function(i) updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  report_tab_no <- tab_name_to_number('report')
  for (tab in global$tabs) {
    # "sum" below is a trick to include NULL case as sum(NULL)=0
    if (sum(tab$next_tab) == report_tab_no) {
      tab$server(input, output, session, switch_page, allow_report)
    } else {
      tab$server(input, output, session, switch_page)
    }
  }
}
