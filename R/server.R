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
  session$userData$temp_md_dev_2 <- tempfile(fileext = ".md")
  session$userData$temp_rtf_dev_2 <- tempfile(fileext = ".rtf")
  if (global$progress_bar){
    # progress bar code
    progress <- Progress$new(session, min=0, max=1, style="old")
    observeEvent(input$wizard, {
      which_tab <- which(input$wizard == sapply(global$tabs, function(tab) tab$id))
      tab_type <- global$tabs[[which_tab]]$type
      tab_number <- global$tabs[[which_tab]]$tab_number
      if (!is.null(tab_type)){
        matching_type <- sapply(
          global$tabs,
          function(tab) sum(c(is.null(tab$type), tab$type==tab_type))
        )
        den <- sum(matching_type)
        num <- sum(matching_type[1:tab_number]) - 1
        progress$set(value=num/den, message="Questionnaire progress")
      } else {
        den <- length(global$tabs) - 1
        num <- tab_number - 1
        progress$set(value=num/den, message="Questionnaire progress")
      }
    })
  }
  # the reactive variables
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
          warning(paste("Issue with", out[i, ], "No product description for", out$product[i]))
        } else {
          out$product_description[i] <- global$products[[out$product[i]]]$description
          out$product_text[i] <- global$products[[out$product[i]]]$text
        }
      } else if (length(splitted_names[[i]]) > 6) {
        warning(paste0("Unexpectedly large number of underscores in ", out$names[i]))
      }
    }
    out$materiality <- factor(out$values, levels = c("N/A", "Low", "Medium", "High"), ordered = T)
    out$materiality_num <- (as.integer(out$materiality) - 1)^2 + (as.integer(out$materiality) > 2)
    out <- out[!is.na(out$type), ]
    return(out)
  })

  allow_report <- reactive({
    return(nrow(get_inputs(all_inputs(), input$inst_type)))
  })
  
  report_message <- reactive({
    if (input$rep_type == "inst" && input$report_scenario_selection == "") {
      return("Please select a scenario (optionally a sector as well)")
    }
    if (input$rep_type == "sect" && input$report_sector_selection == "") {
      return("Please select a sector")
    } else {
      return("")
    }
  })

  # update the available sectors, only after tab switch
  observeEvent(
    input$wizard,
    {
      if (input$rep_type == "inst"){
        selection_type_filter <- input$inst_type
      } else {
        selection_type_filter <- ""
      }
      updateSelectInput(
        session,
        "report_sector_selection",
        choices=c(
          "",
          unname(sapply(global$exposure_classes, `[[`, i = "name"))[
            names(global$exposure_classes) %in% get_inputs(all_inputs(), selection_type_filter)$item
          ]
        )
      )
    }
  )

  output$html_report_message <- renderText({
    report_message()
  })
  
  output$html_report <- renderUI({
    if (report_message() != "") {
      return("")
    }
    temp_html <- tempfile(fileext = ".html")
    if (input$rep_type == "inst"){
      if (input$report_sector_selection == "") {
        exec_summary_layout <- 1
      } else {
        exec_summary_layout <- 2
      }
      write_report_to_file(
        get_report_contents(
          get_inputs(all_inputs(), input$inst_type, input$report_sector_selection, FALSE),
          global$report_version,
          input$report_scenario_selection,
          FALSE,
          exec_summary_layout
        ),
        session$userData$temp_md_scenario
      )
    } else {
      write_report_to_file(
        get_report_contents(
          get_inputs(all_inputs(), "", input$report_sector_selection, FALSE, "High"),
          global$report_version,
          input$report_scenario_selection,
          FALSE,
          2
        ),
        session$userData$temp_md_scenario
      )
    }
    rmarkdown::render(
      input = session$userData$temp_md_scenario,
      output_file = temp_html,
      output_format = rmarkdown::html_document(
        toc = TRUE,
        toc_float = FALSE,
        toc_depth = 2,
        number_sections = FALSE,
        self_contained = FALSE,
        fig_caption = FALSE
      )
    )
    # replace back the images links
    file_conn <- file(temp_html)
    temp <- readLines(file_conn)
    temp <- gsub(
      system.file("www", package = "climate.narrative"),
      "climate_narrative",
      temp
    )
    if (global$report_version >= 2){
      temp <- gsub(
        "(<h[1-5]?>)(.*)(</h[1-5]?>)",
        "<div class=\"inline\"> \\1\\2\\3 <a href='#top'>&uarr;</a> </div>",
        temp,
        perl=TRUE
      )    
    }
    # add class to images
    if (global$report_version >= 4){
      temp <- gsub(
        '<img ',
        '<img class="reportimage"',
        temp
      )   
    }
    # extract the table of contents
    if (global$sidebar_toc){
      toc_start <- grep("<div id=\"TOC\">", temp)
      div_end <- grep("</div>", temp)
      toc_end <- min(div_end[div_end > toc_start])
      toc <- temp[toc_start:toc_end]
      output$html_report_nav <- renderUI(HTML(toc))
      temp <- temp[-(toc_start:toc_end)]
    }
    writeLines(
      temp,
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
      if (input$rep_type == "inst"){
        if (input$report_sector_selection == "") {
          exec_summary_layout <- 1
        } else {
          exec_summary_layout <- 2
        }
        write_report_to_file(
          get_report_contents(
            get_inputs(all_inputs(), input$inst_type, input$report_sector_selection),
            global$report_version,
            input$report_scenario_selection,
            TRUE,
            exec_summary_layout
          ),
          session$userData$temp_md_scenario_and_commons,
          (global$report_version >= 4)
        )
      } else {
        write_report_to_file(
          get_report_contents(
            get_inputs(all_inputs(), "", input$report_sector_selection, FALSE, "High"),
            global$report_version,
            input$report_scenario_selection,
            TRUE,
            2
          ),
          session$userData$temp_md_scenario_and_commons,
          (global$report_version >= 4)
        )
      }
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
      rtf_postprocess(session$userData$temp_rtf, global$report_version)
      removeModal()
      file.copy(session$userData$temp_rtf, file)
    }
  )

  output$dev_report <- downloadHandler(
    filename = "All_Outputs.rtf",
    content = function(file, res_path = system.file("www", package = "climate.narrative")) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      write_report_to_file(
        get_report_contents(
          all_inputs(),
          global$report_version,
          input$report_scenario_selection,
          TRUE,
          1
        ),
        session$userData$temp_md_dev,
        (global$report_version >= 4)
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
      rtf_postprocess(session$userData$temp_rtf_dev, global$report_version)
      removeModal()
      file.copy(session$userData$temp_rtf_dev, file)
    }
  )

output$dev_report_2 <- downloadHandler(
    filename = "Sectors_Output.rtf",
    content = function(file, res_path = system.file("www", package = "climate.narrative")) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      write_report_to_file(
        get_test_report(),
        session$userData$temp_md_dev_2,
        (global$report_version >= 4)
      )
      fs <- file.size(session$userData$temp_md_dev_2)
      rmarkdown::render(
        input = session$userData$temp_md_dev_2,
        output_file = session$userData$temp_rtf_dev_2,
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
      if (file.size(session$userData$temp_md_dev_2) != fs) stop("Rtf rendering issue - md file invisibly truncated!")
      rtf_postprocess(session$userData$temp_rtf_dev_2, global$report_version)
      removeModal()
      file.copy(session$userData$temp_rtf_dev_2, file)
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
