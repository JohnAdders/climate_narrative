library(R6)
# QuestionTab class definition
# instances are tabs of the questionnaire
# this is a wrapper over any ui and server that handles switching to previous/next tab if relevant
# (if the next_tab/previous_tab argument is NULL there is no button and corresponding server logic)
QuestionTab <- R6Class(
  "QuestionTab",
  public = list(
    tab_name = NULL,
    tab_ui = NULL,
    tab_ui_foot = NULL,
    tab_server = NULL,
    tab_number = NULL,
    previous_tab = NULL,
    next_tab = NULL,
    id = NULL,
    add_footer = NULL,
    add_header = NULL,
    initialize = function(tab_name, previous_tab, next_tab, add_header=TRUE, add_footer=TRUE) {
      # the constructor automatically gets ui, server and foot 
      # from the relevant functions (or makes it empty if the function does not exist)
      self$tab_name <- tab_name
      self$tab_ui <- get_or_null(paste0('tab_',tab_name,'_ui'))
      self$tab_ui_foot <- get_or_null(paste0('tab_',tab_name,'_foot'))
      self$tab_server <- get_or_null(paste0('tab_',tab_name,'_server'))
      self$tab_number <- as.integer(factor(tab_name, ordered_tabs))
      self$previous_tab <- as.integer(factor(previous_tab, ordered_tabs))
      self$next_tab <- as.integer(factor(next_tab, ordered_tabs))
      self$id <- paste0("page_", self$tab_number)
      self$add_header <- add_header
      self$add_footer <- add_footer
    },
    # tab server function that combines:
    # 1. any other server tab_server (if given in the constructor)
    # 2. possibility of switch to previous/next tab (if applicable), using 'switch_page' function.
    server = function(input, output, session, switch_page) {
      switch_page <- function(i) updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
      if (!is.null(self$tab_server)) self$tab_server(input, output, session, self)
      if (length(self$previous_tab)) observeEvent(
        input[[paste0(self$id,"_previous")]], switch_page(as.integer(self$previous_tab)))
      if (length(self$next_tab)) observeEvent(
        input[[paste0(self$id,"_next")]], switch_page(as.integer(self$next_tab)))
    },
    # tab UI function combines:
    # 0. a common header
    # 1. any other tab_UI (if given in the constructor)
    # 2. buttons that switch to previous/next tab (if applicable)
    # 3. a tab-specific footer
    ui = function() {
      tabpanel_params <- list(self$id)
      # if(self$add_header) tabpanel_params <- add_param(
      #   tabpanel_params,
      #   tag('header',list(
      #     img(src='cfrf_logo.png', alt='CFRF logo', height=50)
      #   ))
      # )
      if (!is.null(self$tab_ui)) tabpanel_params <- add_param(
        tabpanel_params, self$tab_ui())
      tabpanel_params <- add_param(tabpanel_params, br())
      if (length(self$previous_tab)) tabpanel_params = add_param(
        tabpanel_params, actionButton(paste0(self$id, "_previous"), "prev"))
      if (length(self$next_tab)) tabpanel_params = add_param(
        tabpanel_params, actionButton(paste0(self$id, "_next"), "next"))
      if (!is.null(self$tab_ui_foot)) tabpanel_params <- add_param(
        tabpanel_params, self$tab_ui_foot())
      
      # if(self$add_footer) tabpanel_params <- add_param(
      #   tabpanel_params, heartbeat_footer()
      # )
      do.call(tabPanel, tabpanel_params)
    }
  ),
)