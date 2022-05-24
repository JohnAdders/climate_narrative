tab_report_ui <- function() {
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc (required to render rtf) not available")
  }
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
    "Select the sector/asset class to show",
    sector_options,
    selectize = FALSE
  )
  button_1_conditional <- conditionalPanel(
    'output.html_report_message == ""',
    actionButton(paste0("page_", tab_name_to_number("report"), "_previous"), "prev"),
    class = "inline2"
  )
  button_1_unconditional <- actionButton(paste0("page_", tab_name_to_number("report"), "_previous"), "prev")
  button_2 <- conditionalPanel(
    'output.html_report_message == ""',
    downloadButton("report", "Download as RTF"),
    class = "inline2"
  )
  if (global$dev) {
    button_3 <- actionButton("update_yamls", "Update the report text files")
    dropdown_3 <- selectInput(
      "version_selection",
      "Select the report version",
      global$report_versions,
      global$report_version,
      selectize = FALSE
    )
    dropdown_part <- div(
      class = "not_too_wide_3col",
      fluidRow(
        column(4, dropdown_1),
        column(4, dropdown_2),
        column(4, dropdown_3)
      ),
      fluidRow(
        column(4, button_1_conditional),
        column(4, button_2),
        column(4, button_3)
      )
    )
  } else {
    dropdown_part <- div(
      class = "not_too_wide_2col",
      fluidRow(
        column(6, dropdown_1),
        column(6, dropdown_2)
      ),
      fluidRow(
        column(4, button_1_conditional),
        column(4, button_2)
      )
    )
  }

  if (global$report_version >= 6) {
    if (global$dev) {
      button_part <- column(8, button_1_unconditional, button_2, button_3)
    } else {
      button_part <- column(8, button_1_unconditional, button_2)
    }
    head_part <- tags$header(
      fluidRow(
        column(4, img(src = "climate_narrative/cfrf_logo.png", alt = "CFRF logo", height = 50, class = "inline2")),
        button_part
      )
    )
    head_part <- tagAppendAttributes(head_part, class = "report_head")
    main_part <- sidebarLayout(
      tagAppendAttributes(
        sidebarPanel(
          dropdown_1,
          hr(),
          dropdown_2,
          hr(),
          uiOutput("html_report_nav")
        ),
        class = "h100"
      ),
      mainPanel(
        textOutput("html_report_message"),
        uiOutput("html_report"),
        heartbeat_footer()
      )
    )
    main_part <- tagAppendAttributes(main_part, class = "report_main")
    out <- div(
      class = "report_page",
      head_part,
      main_part
    )
  } else {
    main_part <- div(
      class = "bottomlayer",
      list(
        textOutput("html_report_message"),
        uiOutput("html_report")
      )
    )
    head <- tags$header(
      img(src = "climate_narrative/cfrf_logo.png", alt = "CFRF logo", height = 50),
      dropdown_part
    )
    out <- list(
      head,
      main_part
    )
  }
}

tab_report_server <- function(input, output, session, tab) {
  observeEvent(
    list(input$inst_type, input$rep_type),
    {
      if (input$rep_type == "inst") {
        tab$previous_tab <- tab_name_to_number(
          switch(input$inst_type,
            insurance = "ins_re",
            asset = "am_re",
            bank = "bank_sov"
          )
        )
        updateSelectInput(
          session = session,
          inputId = "report_sector_selection",
          label = switch(input$inst_type,
            insurance = "Select the sector/asset class/liability class to show",
            asset = "Select the sector/asset class to show",
            bank = "Select the sector/asset class to show"
          )
        )
      } else {
        tab$previous_tab <- tab_name_to_number("rep_type")
        updateSelectInput(
          session = session,
          inputId = "report_sector_selection",
          label = "Select the sector/asset class/liability class to show"
        )
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
