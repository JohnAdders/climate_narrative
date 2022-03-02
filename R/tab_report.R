tab_report_ui <- function() {
  scenario_options <- c(
    "",
    unname(unlist(lapply(global$scenarios, function(x) x$name)))
  )
  dropdown_1 <- selectInput(
    "report_scenario_selection",
    "Select the scenario to show",
    scenario_options,
    selectize = FALSE
  )
  sector_options <- ""
  dropdown_2 <- selectInput(
    "report_sector_selection",
    "Select the sector to show",
    sector_options,
    selectize = FALSE
  )

  if (global$dev) {
    dropdown_3 <- selectInput(
      "version_selection",
      "Select the report version",
      global$report_versions,
      global$report_version,
      selectize = FALSE
    )
    out <- list(
      fluidRow(
        column(4, dropdown_1),
        column(4, dropdown_2),
        column(4, dropdown_3)
      )
    )
  } else {
    out <- list(
      fluidRow(
        column(6, dropdown_1),
        column(6, dropdown_2)
      )
    )
  }
  if (!rmarkdown::pandoc_available()) {
    warning("Pandoc (required to render rtf) not available, hiding download report button")
    out <- c(out, list(uiOutput("html_report")))
  } else {
    button_1 <- conditionalPanel(
      'output.html_report_message == ""',
      actionButton(paste0("page_", tab_name_to_number("report"), "_previous_duplicate"), "prev")
    )
    button_2 <- conditionalPanel(
      'output.html_report_message == ""',
      downloadButton("report", "Download the selected report as RTF")
    )
    button_3 <- actionButton("update_yamls", "Update the report text files")
    if (global$dev) {
      out <- c(out, list(
        fluidRow(
          column(4, button_1),
          column(4, button_2),
          column(4, button_3)
        )
      ))
    } else {
      out <- c(out, list(
        fluidRow(
          column(4, button_1),
          column(4, button_2)
        )
      ))
    }
    out <- c(out, 
      list(
        textOutput("html_report_message"),
        uiOutput("html_report")
      )
    )
  }
}

tab_report_server <- function(input, output, session, tab) {
  if (global$dev == TRUE) {
    updateSelectInput(session, "report_scenario_selection", selected = global$scenarios[[2]]$name)
  }
  observeEvent(
    list(input$inst_type, input$rep_type),
    {
      if (input$rep_type == "inst") {
        tab$previous_tab <- tab_name_to_number(
          switch(input$inst_type,
            insurance = "ins_sov",
            asset = "am_re",
            bank = "bank_sov"
          )
        )
      } else {
        tab$previous_tab <- tab_name_to_number("rep_type")
      }
    }
  )
  observeEvent(
    list(
      input[[paste0("page_", tab_name_to_number("report"), "_previous")]],
      input[[paste0("page_", tab_name_to_number("report"), "_previous_duplicate")]]
    ),
    {
      updateSelectInput(session, "report_scenario_selection", selected = "")
      updateSelectInput(session, "report_sector_selection", selected = "")
    }
  )
  observeEvent(
    input$version_selection,
    {
      global$report_version <- input$version_selection
    }
  )
  observeEvent(
    input$update_yamls,
    {
      global$exposure_classes <- read_dir("exposure_class")
      global$exposures <- read_dir("exposure")
      global$scenarios <- read_dir("scenario")
      global$section <- read_dir("section")
      global$products <- read_dir("product")
    }
  )
}
