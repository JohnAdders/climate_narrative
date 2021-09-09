library(R6)
# QuestionTab class definition
# instances are tabs of the questionnaire
# this is a wrapper over any ui and server that handles switching to previous/next tab if relevant
# (if the next_tab/previous_tab argument is NULL there is no button and corresponding server logic)
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
    # tab server function that combines:
    # 1. any other server tab_server (if given in the constructor)
    # 2. possibility of switch to previous/next tab (if applicable), using 'switch_page' function. 
    server = function(input, output, session, switch_page) {
      
      switch_page <- function(i) {
        updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
      }
      if (!is.null(self$tab_server)) self$tab_server(input, output, session, self)
      if (!is.null(self$previous_tab)) observeEvent(
        input[[paste0(self$id,"_previous")]], switch_page(self$previous_tab))
      if (!is.null(self$next_tab)) {
        observeEvent(input[[paste0(self$id,"_next")]],{
          # the line below - moved to tab6 instead of hardcoding next_tab==6 here
          # if(self$next_tab == 6) update_final_page(input, output, session)
          switch_page(self$next_tab)
        }
        )
      }
    },
    # tab UI function combines:
    # 1. any other tab_UI (if given in the constructor)
    # 2. buttons that switch to previous/next tab (if applicable)
    ui = function() {
      tabpanel_params <- list(self$id)
      if (!is.null(self$tab_ui)) tabpanel_params <- add_param(tabpanel_params, self$tab_ui())
      tabpanel_params = add_param(tabpanel_params, br())
      if (!is.null(self$previous_tab)) tabpanel_params = add_param(
        tabpanel_params, actionButton(paste0(self$id,"_previous"), "prev"))
      if (!is.null(self$next_tab)) tabpanel_params = add_param(
        tabpanel_params, actionButton(paste0(self$id,"_next"), "next"))
      do.call(tabPanel, tabpanel_params)
    }
  )
)