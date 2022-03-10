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
  print(system.file("www", package = "climate.narrative"))
  print(dir(system.file("www", package = "climate.narrative"))
  heartbeat(input, output, session)
  session$userData$verification_code <- substring(uuid::UUIDgenerate(), 1, 6)
  session$userData$captcha_validated <- FALSE
  session$userData$temp_md_full <- tempfile(fileext = ".md")
  session$userData$temp_md_scenario <- tempfile(fileext = ".md")
  session$userData$temp_md_scenario_and_commons <- tempfile(fileext = ".md")
  session$userData$temp_html <- tempfile(fileext = ".html")
  session$userData$temp_rtf <- tempfile(fileext = ".rtf")
  session$userData$temp_md_dev <- tempfile(fileext = ".md")
  session$userData$temp_rtf_dev <- tempfile(fileext = ".rtf")
  session$userData$temp_md_dev_2 <- tempfile(fileext = ".md")
  session$userData$temp_rtf_dev_2 <- tempfile(fileext = ".rtf")
  if (global$progress_bar) {
    # progress bar code
    progress <- Progress$new(session, min = 0, max = 1, style = "old")
    observeEvent(input$wizard, {
      which_tab <- which(input$wizard == sapply(global$tabs, function(tab) tab$id))
      tab_type <- global$tabs[[which_tab]]$type
      tab_number <- global$tabs[[which_tab]]$tab_number
      if (!is.null(tab_type)) {
        matching_type <- sapply(
          global$tabs,
          function(tab) sum(c(is.null(tab$type), tab$type == tab_type))
        )
        den <- sum(matching_type)
        num <- sum(matching_type[1:tab_number]) - 1
        progress$set(value = num / den, message = "Questionnaire progress")
      } else {
        den <- length(global$tabs) - 1
        num <- tab_number - 1
        progress$set(value = num / den, message = "Questionnaire progress")
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
    if (input$wizard != paste0("page_", tab_name_to_number("report"))) {
      return("Tab not visible")
    }
    if (input$rep_type == "inst" && input$report_scenario_selection == "") {
      return("Please select a scenario (optionally a sector as well)")
    }
    if (input$rep_type == "sect" && input$report_sector_selection == "") {
      return("Please select a sector")
    } else {
      return("")
    }
  })

  # update the available sectors
  observeEvent(
    list(
      input$rep_type,
      input$inst_type
    ),
    {
      if (input$rep_type == "inst") {
        selection_type_filter <- input$inst_type
        name_of_blank_scenario <- ""
        name_of_blank_sector <- "All relevant sectors"
      } else {
        selection_type_filter <- ""
        name_of_blank_scenario <- "All relevant scenarios"
        name_of_blank_sector <- ""
      }
      sectors_available <- (names(global$exposure_classes) %in% get_inputs(all_inputs(), selection_type_filter)$item)
      sector_choices <- c(
        "",
        names(sapply(global$exposure_classes, `[[`, i = "name"))[sectors_available]
      )
      names(sector_choices) <- c(
        name_of_blank_sector,
        unname(sapply(global$exposure_classes, `[[`, i = "name"))[sectors_available]
      )
      scenario_options <- c(
        "",
        unname(unlist(lapply(global$scenarios, function(x) x$name)))
      )
      names(scenario_options) <- scenario_options
      names(scenario_options)[1] <- name_of_blank_scenario
      updateSelectInput(
        session,
        "report_sector_selection",
        choices = sector_choices
      )
      updateSelectInput(
        session,
        "report_scenario_selection",
        choices = scenario_options
      )
    }
  )

  output$html_report_message <- renderText({
    report_message()
  })

  observeEvent(
    list(
      input$wizard == paste0("page_", tab_name_to_number("report")),
      input$report_scenario_selection, input$report_sector_selection, report_message()
    ),
    {
      if (report_message() != "") {
        output$html_report <- renderUI("") # used to be: return("")
      } else {
        temp_html <- session$userData$temp_html
        showModal(
          modalDialog(
            "Report rendering in progress... when complete it will show automatically",
            title = "Climate Report",
            footer = NULL
          )
        )
        if (global$report_version >= 5) {
          settings <- get_report_settings(global$content_files, temp_html, session$userData$temp_md_scenario, "html", global$report_version, global$sidebar_toc, input$rep_type, input$inst_type, input$report_sector_selection, input$report_scenario_selection)
          produce_report(all_inputs(), settings)
          result <- includeHTML(temp_html)
          removeModal()
          output$html_report <- renderUI(result) # used to be: return(result)
        } else { # old code below
          if (input$rep_type == "inst") {
            inputs <- get_inputs(all_inputs(), input$inst_type, input$report_sector_selection, FALSE)
            include_exposures <- TRUE
            if (input$report_sector_selection == "") {
              exec_summary_layout <- 1
            } else {
              exec_summary_layout <- 2
            }
          } else {
            exec_summary_layout <- 2
            inputs <- get_inputs(all_inputs(), "", input$report_sector_selection, FALSE, "High")
            include_exposures <- FALSE
          }
          if (!global$sidebar_toc) {
            output_format <- rmarkdown::html_document(
              toc = TRUE,
              toc_float = FALSE,
              toc_depth = 2,
              number_sections = FALSE,
              self_contained = FALSE,
              fig_caption = FALSE
            )
          } else {
            output_format <- rmarkdown::html_document(
              toc = TRUE,
              toc_float = list(collapsed = FALSE),
              theme = "sandstone",
              toc_depth = 2,
              number_sections = FALSE,
              self_contained = TRUE,
              fig_caption = FALSE
            )
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
            session$userData$temp_md_scenario,
            (global$report_version >= 4)
          )
          render_html(session$userData$temp_md_scenario, temp_html, global$report_version, global$sidebar_toc)
          result <- includeHTML(temp_html)
          removeModal()
          output$html_report <- renderUI(result) # used to be: return(result)
        }
      }
    }
  )

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
      if (global$report_version >= 5) {
        settings <- get_report_settings(global$content_files, session$userData$temp_rtf, session$userData$temp_md_scenario_and_commons, "rtf", global$report_version, global$sidebar_toc, input$rep_type, input$inst_type, input$report_sector_selection, input$report_scenario_selection)
        produce_report(all_inputs(), settings)
        removeModal()
        file.copy(session$userData$temp_rtf, file)
      } else { # old code below
        if (input$rep_type == "inst") {
          inputs <- get_inputs(all_inputs(), input$inst_type, input$report_sector_selection)
          include_exposures <- TRUE
          if (input$report_sector_selection == "") {
            exec_summary_layout <- 1
          } else {
            exec_summary_layout <- 2
          }
        } else {
          exec_summary_layout <- 2
          inputs <- get_inputs(all_inputs(), "", input$report_sector_selection, FALSE, "High")
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
      if (global$report_version >= 5) {
        settings <- get_report_settings(global$content_files, session$userData$temp_rtf_dev, session$userData$temp_md_dev, "rtf", global$report_version, global$sidebar_toc, "inst", "", "", "")
        produce_report(all_inputs(), settings)
        removeModal()
        file.copy(session$userData$temp_rtf_dev, file)
      } else {
        temp <- get_report_contents(
          global$tabs,
          global$scenarios,
          global$sections,
          global$exposure_classes,
          all_inputs(),
          global$report_version,
          input$report_scenario_selection,
          TRUE,
          1,
          TRUE
        )
        write_report_to_file(
          temp,
          session$userData$temp_md_dev,
          (global$report_version >= 4)
        )
        render_rtf(session$userData$temp_md_dev, session$userData$temp_rtf_dev, res_path, global$report_version)
        removeModal()
        file.copy(session$userData$temp_rtf_dev, file)
      }
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
      if (global$report_version >= 5) {
        settings <- get_report_settings(global$content_files, session$userData$temp_rtf_dev_2, session$userData$temp_md_dev_2, "rtf", global$report_version, global$sidebar_toc, "test", "", "", "")
        produce_report(NULL, settings)
        removeModal()
        file.copy(session$userData$temp_rtf_dev_2, file)
      } else {
        temp <- get_test_report(global$exposure_classes)
        write_report_to_file(
          temp,
          session$userData$temp_md_dev_2,
          (global$report_version >= 4)
        )
        render_rtf(session$userData$temp_md_dev_2, session$userData$temp_rtf_dev_2, res_path, global$report_version)
        removeModal()
        file.copy(session$userData$temp_rtf_dev_2, file)
      }
    }
  )

  # finally, tab-specific server function collation
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }
  report_tab_no <- tab_name_to_number("report")
  for (tab in global$tabs) {
    # "sum" below is a trick to include NULL case as sum(NULL)=0
    if (sum(tab$next_tab) == report_tab_no) {
      tab$server(input, output, session, switch_page, allow_report)
    } else {
      tab$server(input, output, session, switch_page)
    }
  }
}
