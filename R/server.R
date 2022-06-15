#' Main Shiny server function
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @importFrom stats aggregate
#' @importFrom promises %...>% %...!%
#' @export
#'
server <- function(input, output, session) {
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
    new_col_names <- c("type", "subtype", "rowname", "product", "colname", "item", "product_description", "product_text", "A_or_L")
    out <- cbind(out, matrix(NA, nrow = nrow(out), ncol = length(new_col_names)))
    colnames(out) <- c("names", "values", new_col_names)
    splitted_names <- strsplit(out$names, "_", fixed = TRUE)
    for (i in 1:nrow(out)) {
      if (length(splitted_names[[i]]) == 6) {
        out[i, 3:8] <- splitted_names[[i]]
        if (is.null(global$products[[out$product[i]]])) {
          warning(paste("Issue with", out[i, ], "No product description for", out$product[i], "\n"))
        } else {
          out$product_description[i] <- global$products[[out$product[i]]]$description
          out$product_text[i] <- global$products[[out$product[i]]]$text
          out$A_or_L[i] <- global$products[[out$product[i]]]$A_or_L
        }
      } else if (length(splitted_names[[i]]) > 6) {
        warning(paste0("Unexpectedly large number of underscores in ", out$names[i]))
      }
    }
    out$materiality <- factor(out$values, levels = c("N/A", "Low", "Medium", "High"), ordered = T)
    out$materiality_num <- (as.integer(out$materiality) - 1)^2 + (as.integer(out$materiality) > 2)
    out$exposure_group <- sapply(out$item, function(class) {
      group_or_null <- global$exposure_classes[[class]]$group
      if (is.null(group_or_null)) {
        return("")
      } else {
        return(group_or_null)
      }
    })
    out <- out[!is.na(out$type), ]
    return(out)
  })
  allow_report <- reactive({
    return(
      any(
        input$rep_type == "sect",
        nrow(
          filter_inputs(
            all_inputs(),
            list(
              inst_type = input$inst_type,
              report_sector_selection = "",
              override_materiality = ""
            )
          )
        )
      )
    )
  })

  observeEvent(
    allow_report(),
    {
      if (allow_report()) {
        session$userData$next_tabs$bank_sov <- tab_name_to_number("report")
        session$userData$next_tabs$ins_re <- tab_name_to_number("report")
        session$userData$next_tabs$am_re <- tab_name_to_number("report")
      } else {
        session$userData$next_tabs$bank_sov <- tab_name_to_number("bank_sov")
        session$userData$next_tabs$ins_re <- tab_name_to_number("ins_re")
        session$userData$next_tabs$am_re <- tab_name_to_number("am_re")
      }
    }
  )

  report_message <- reactive({
    req(input$wizard, input$rep_type)
    if (input$wizard != paste0("page_", tab_name_to_number("report"))) {
      return("Tab not visible")
    }
    if (input$rep_type == "inst" && input$report_scenario_selection == "") {
      return("Please select a scenario (optionally a sector as well)")
    }
    if (input$rep_type == "sect" && input$report_sector_selection == "" && input$report_scenario_selection == "") {
      return("Please select a sector (or a scenario for a scenario-only report)")
    } else {
      return("")
    }
  })

  # update the available sectors
  observeEvent(
    input$wizard == paste0("page_", tab_name_to_number("report")),
    {
      req(input$wizard, input$rep_type)
      if (input$rep_type == "inst") {
        selection_type_filter <- input$inst_type
        name_of_blank_scenario <- ""
        name_of_blank_sector <- "All relevant sectors"
      } else {
        selection_type_filter <- ""
        name_of_blank_scenario <- "All relevant scenarios"
        name_of_blank_sector <- ""
      }
      all_relevant_inputs <- filter_inputs(
        all_inputs(),
        list(
          inst_type = selection_type_filter,
          report_sector_selection = "",
          override_materiality = ""
        )
      )
      sectors_available <- (names(global$exposure_classes) %in% all_relevant_inputs$item)
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
        output$html_report <- renderUI("")
        if (global$report_version >= 6) {
          output$html_report_nav <- renderUI("")
        }
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
          settings <- get_report_settings(global$content_files, temp_html, session$userData$temp_md_scenario, "html", global$report_version, input$rep_type, input$inst_type, input$report_sector_selection, input$report_scenario_selection)
          if (global$report_version >= 7) {
            produce_report(all_inputs(), settings, TRUE) %...>% {
              removeModal()
              result <- includeHTML(temp_html)
              output$html_report <- renderUI(result)
              output$html_report_nav <- renderUI(includeHTML(paste0(substr(temp_html, 1, nchar(temp_html) - 5), "_toc.html")))
            }
          } else {
            produce_report(all_inputs(), settings)
            removeModal()
            result <- includeHTML(temp_html)
            output$html_report <- renderUI(result)
            if (global$report_version >= 6) {
              output$html_report_nav <- renderUI(includeHTML(paste0(substr(temp_html, 1, nchar(temp_html) - 5), "_toc.html")))
            }
          }
        } else {
          stop("Error. Report version < 5 removed")
        }
      }
    }
  )

  # download button inspired by: https://shiny.rstudio.com/articles/generating-reports.html
  output$report <- downloadHandler(
    filename = "Climate Report.rtf",
    content = function(file, res_path = ifelse(system.file("www", package = "climate.narrative") == "", "inst/www", system.file("www", package = "climate.narrative"))) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      if (global$report_version >= 5) {
        settings <- get_report_settings(global$content_files, session$userData$temp_rtf, session$userData$temp_md_scenario_and_commons, "rtf", global$report_version, input$rep_type, input$inst_type, input$report_sector_selection, input$report_scenario_selection)
        if (global$report_version >= 7) {
          produce_report(all_inputs(), settings, TRUE) %...>% {
            removeModal()
            file.copy(session$userData$temp_rtf, file)
          }
        } else {
          produce_report(all_inputs(), settings)
          removeModal()
          file.copy(session$userData$temp_rtf, file)
        }
      } else { # old code below
        stop("Error. Report version < 5 removed")
      }
    }
  )

  output$dev_report <- downloadHandler(
    filename = "All_Outputs.rtf",
    content = function(file, res_path = ifelse(system.file("www", package = "climate.narrative") == "", "inst/www", system.file("www", package = "climate.narrative"))) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      if (global$report_version >= 5) {
        settings <- get_report_settings(global$content_files, session$userData$temp_rtf_dev, session$userData$temp_md_dev, "rtf", global$report_version, "inst", "", "", "")
        if (global$report_version >= 7) {
          produce_report(all_inputs(), settings, TRUE) %...>% {
            removeModal()
            file.copy(session$userData$temp_rtf_dev, file)
          }
        } else {
          produce_report(all_inputs(), settings)
          removeModal()
          file.copy(session$userData$temp_rtf_dev, file)
        }
      } else {
        stop("Error. Report version < 5 removed")
      }
    }
  )

  output$dev_report_2 <- downloadHandler(
    filename = "Sectors_Output.rtf",
    content = function(file, res_path = ifelse(system.file("www", package = "climate.narrative") == "", "inst/www", system.file("www", package = "climate.narrative"))) {
      showModal(
        modalDialog(
          "Report rendering in progress... when complete your download will start automatically",
          title = "Climate Report",
          footer = NULL
        )
      )
      if (global$report_version >= 5) {
        settings <- get_report_settings(global$content_files, session$userData$temp_rtf_dev_2, session$userData$temp_md_dev_2, "rtf", global$report_version, "test", "", "", "")
        if (global$report_version >= 7) {
          produce_report(all_inputs(), settings, TRUE) %...>% {
            removeModal()
            file.copy(session$userData$temp_rtf_dev_2, file)
          }
        } else {
          produce_report(NULL, settings)
          removeModal()
          file.copy(session$userData$temp_rtf_dev_2, file)
        }
      } else {
        stop("Error. Report version < 5 removed")
      }
    }
  )

  # finally, tab-specific server function collation
  switch_page <- function(i) {
    i <- as.integer(i)
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }

  for (tab in global$tabs) {
    tab$server(input, output, session, switch_page)
  }

  # saving previous/next tab to session in order to make them dynamic (and not interfering with other sessions via global)
  session$userData$prev_tabs <- lapply(global$tabs, function(x) sum(x$initial_previous_tab))
  session$userData$next_tabs <- lapply(global$tabs, function(x) sum(x$initial_next_tab))
  names(session$userData$prev_tabs) <- names(session$userData$next_tabs) <- sapply(global$tabs, function(x) x$tab_name)
}
