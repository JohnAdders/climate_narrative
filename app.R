library(shiny)
library(R6)

add_param <- function(previous_list, iten_to_add) {
  c(previous_list, list(iten_to_add))
}


update_final_page <- function(input, output, session) {

  summary = list(h1("Climate Narrative"))

  summary[[length(summary)+1]] <- p("Some Text.")

  summary[[length(summary)+1]] <- p("Some more Text.")

  summary[[length(summary)+1]] <- p("Yet more Text.")

  output$summary <- renderUI(summary)
}


QuestionTab <- R6Class(
  "QuestionTab",
  public = list(
    tab_ui = NULL,
    tab_server = NULL,
    tab_number = NULL,
    previous_tab = NULL,
    next_tab = NULL,
    id = NULL,
    initialize = function(tab_ui, tab_server, tab_number, previous_tab, next_tab) {
      self$tab_ui <- tab_ui
      self$tab_server <- tab_server
      self$tab_number <- tab_number
      self$previous_tab <- previous_tab
      self$next_tab <- next_tab
      self$id <- paste0("page_", tab_number)
    },
    server = function(input, output, session, switch_page) {
      if (!is.null(self$tab_server)) {
        self$tab_server(input, output, session, self)
      }

      if (!is.null(self$previous_tab)) {
        observeEvent(input[[paste0(self$id,"_previous")]], switch_page(self$previous_tab))
      }
      if (!is.null(self$next_tab)) {
        observeEvent(
          input[[paste0(self$id,"_next")]],
          {
            if(self$next_tab == 6) {
              update_final_page(input, output, session)
            }
            switch_page(self$next_tab)
          }
        )
      }
    },
    ui = function() {
      tabpanel_params <- list(self$id)

      if (!is.null(self$tab_ui)) {
        tabpanel_params <- add_param(tabpanel_params, self$tab_ui())
      }

      tabpanel_params = add_param(tabpanel_params, br())

      if (!is.null(self$previous_tab)) {
        tabpanel_params = add_param(tabpanel_params, actionButton(paste0(self$id,"_previous"), "prev"))
      }
      if (!is.null(self$next_tab)) {
        tabpanel_params = add_param(tabpanel_params, actionButton(paste0(self$id,"_next"), "next"))
      }

      do.call(tabPanel, tabpanel_params)
    }
  )
)

bank_exposures <- read.csv("bank_exposures.csv", stringsAsFactors = FALSE)


exposure_grid_cell <- function(exposure_item) {
  if(exposure_item == "") {
    return (column(1, p("")))
  } else {
    return (column(1, p("H/M/L")))
  }
}

exposure_grid_row <- function(exposures_row) {
  return (do.call(fluidRow, lapply(exposures_row, exposure_grid_cell)))
}

exposure_grid <- function(exposures) {
  headings = colnames(exposures)
  rows = c(
    list(
      do.call(
        fluidRow,
        lapply(
          colnames(bank_exposures),
          function(header) {
            column(1, h4(header))
          }
        )
      )
    ),
    apply(exposures, 1, exposure_grid_row)
  )
  browser()
  return (rows)
}


tab1_ui <- function () {
  list(
      radioButtons(
      "type",
      "Type of Institution:",
      c(
        "Bank" = "bank",
        "Insurance" = "insurance",
        "Asset Manager" = "asset"
      )
    )
  )
}

tab2_ui <- function () {
  result = list(
    h2("Bank Exposures")
  )
  result = c(result, exposure_grid(bank_exposures))
  return (result)
}

tab3_ui <- function () {
  list(h2("Insurance Asset Exposures"))
}

tab4_ui <- function () {
  list(h2("Asset Manager Holdings"))
}

tab5_ui <- function () {
  list(h2("Insurance Liabilties"))
}

tab6_ui <- function () {
  list(
    uiOutput("summary")
  )
}

tab1_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$next_tab <- switch(
        input$type,
        insurance = 3,
        asset = 4,
        2
      )
    }
  )
}

tab6_server <- function (input, output, session, tab) {
  observeEvent(
    input$type,
    {
      tab$previous_tab <- switch(
        input$type,
        insurance = 5,
        asset = 3,
        1
      )
    }
  )
}

tabs <- list(
  QuestionTab$new(tab1_ui, tab1_server, 1, NULL, 2),
  QuestionTab$new(tab2_ui, NULL, 2, 1, 6),
  QuestionTab$new(tab3_ui, NULL, 3, 1, 5),
  QuestionTab$new(tab4_ui, NULL, 4, 1, 6),
  QuestionTab$new(tab5_ui, NULL, 5, 3, 6),
  QuestionTab$new(tab6_ui, tab6_server, 6, 2, NULL)
)

ui <- function() {
  tabset_start <- list(
    id = "wizard",
    type = "hidden"
  )

  tabset_tabs <- lapply(tabs, function(tab) {tab$ui()})

  fluidPage(
    do.call(tabsetPanel, c(tabset_start, tabset_tabs))
  )
}

server <- function(input, output, session) {
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
  }

  for (tab in tabs) {
    tab$server(input, output, session, switch_page)
  }
}

shinyApp(ui = ui, server = server)
