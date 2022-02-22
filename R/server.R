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
    exposure_classes_names <- sapply(global$exposure_classes, `[[`, i = "name")
    return(nrow(get_inputs(exposure_classes_names, all_inputs(), input$inst_type)))
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
      exposure_classes_names <- sapply(global$exposure_classes, `[[`, i = "name")
      updateSelectInput(
        session,
        "report_sector_selection",
        choices=c(
          "",
          unname(sapply(global$exposure_classes, `[[`, i = "name"))[
            names(global$exposure_classes) %in% get_inputs(
                exposure_classes_names,
                all_inputs(),
                selection_type_filter
              )$item
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
    if (global$refactor){
      print("settings")
      settings <- get_report_settings("html", global$report_version, input$rep_type, input$inst_type, input$report_sector_selection, input$report_scenario_selection)
      print("report")
      produce_report(all_inputs(), settings)
      print("to browser")
      result <- includeHTML("C:/Users/kopalski/Desktop/temp/temp.html")
      return(result)
    } # old code below
    exposure_classes_names <- sapply(global$exposure_classes, `[[`, i = "name")
    temp_html <- tempfile(fileext = ".html")
    if (input$rep_type == "inst"){
      inputs <- get_inputs(exposure_classes_names, all_inputs(), input$inst_type, input$report_sector_selection, FALSE)
      include_exposures <- TRUE
      if (input$report_sector_selection == "") {
        exec_summary_layout <- 1
      } else {
        exec_summary_layout <- 2
      }
    } else {
      exec_summary_layout <- 2
      inputs <- get_inputs(exposure_classes_names, all_inputs(), "", input$report_sector_selection, FALSE, "High")
      include_exposures <- FALSE
    }
    write_report_to_file(
      get_report_contents(
        global$tabs,
        global$scenarios,
        global$sections,
        global$exposure_classes,
        inputs,
        global$report_version,
        input$report_scenario_selection,
        FALSE,
        exec_summary_layout,
        include_exposures
      ),
      session$userData$temp_md_scenario
    )
    render_html(session$userData$temp_md_scenario, temp_html, global$report_version, global$sidebar_toc)
    result <- includeHTML(temp_html)
    return(result)
  })

  observeEvent(input$testbutton, {
    output$test <- renderUI({
      settings <- get_report_settings("html", 4, "inst", "bank", "", "")
      produce_report(all_inputs(), settings)
      result <- includeHTML("C:/Users/kopalski/Desktop/temp/temp.html")
      return(result)
    })
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
      exposure_classes_names <- sapply(global$exposure_classes, `[[`, i = "name")
      if (input$rep_type == "inst"){
        inputs <- get_inputs(exposure_classes_names, all_inputs(), input$inst_type, input$report_sector_selection)
        include_exposures <- TRUE
        if (input$report_sector_selection == "") {
          exec_summary_layout <- 1
        } else {
          exec_summary_layout <- 2
        }
      } else {
        exec_summary_layout <- 2
        inputs <- get_inputs(exposure_classes_names, all_inputs(), "", input$report_sector_selection, FALSE, "High")
        include_exposures <- FALSE
      }
      write_report_to_file(
        get_report_contents(
          inputs,
          global$report_version,
          input$report_scenario_selection,
          TRUE,
          exec_summary_layout,
          include_exposures
        ),
        session$userData$temp_md_scenario_and_commons,
        (global$report_version >= 4)
      )
      render_rtf(session$userData$temp_md_scenario_and_commons, session$userData$temp_rtf, res_path, global$report_version)
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
          1,
          TRUE
        ),
        session$userData$temp_md_dev,
        (global$report_version >= 4)
      )
      render_rtf(session$userData$temp_md_dev, session$userData$temp_rtf_dev, res_path, global$report_version)
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
        get_test_report(global$exposure_classes),
        session$userData$temp_md_dev_2,
        (global$report_version >= 4)
      )
      render_rtf(session$userData$temp_md_dev_2, session$userData$temp_rtf_dev_2, res_path, global$report_version)
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
