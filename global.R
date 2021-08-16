library(R6)
source('table_functions.R')
    
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
insurance_assets <- read.csv("insurance_assets.csv", stringsAsFactors = FALSE)
insurance_liabilities <- read.csv("insurance_liabilities.csv", stringsAsFactors = FALSE)
am_exposures <- read.csv("am_exposures.csv", stringsAsFactors = FALSE)

tabs <- list(
    QuestionTab$new(tab1_ui, tab1_server, 1, NULL, 2),
    QuestionTab$new(tab2_ui, NULL, 2, 1, 6),
    QuestionTab$new(tab3_ui, NULL, 3, 1, 5),
    QuestionTab$new(tab4_ui, NULL, 4, 1, 6),
    QuestionTab$new(tab5_ui, NULL, 5, 3, 6),
    QuestionTab$new(tab6_ui, tab6_server, 6, 2, NULL)
)

add_param <- function(previous_list, iten_to_add) {
  # alternative method that may be used to define a list below
  c(previous_list, list(iten_to_add))
}

update_final_page <- function(input, output, session) {
  summary = list(
    h1("Climate Narrative"),
    p("Some Text."),
    p("Some more Text."),
    p("Yet more Text."),
    p(paste0("Equity exposure to gas sector is: ", input$gas_Equity))
  )
  output$summary <- renderUI(summary)
}