#' QuestionTab class definition
#' 
#' Instances are tabs of the questionnaire.
#' This is a wrapper over any ui and server that handles switching to previous/next tab if relevant
#' (if the next_tab/previous_tab argument is NULL there is no button and corresponding server logic). 
QuestionTab <- R6Class(
  "QuestionTab",
  public = list(
    #' @field tab_name name of the tab
    tab_name = NULL,
    #' @field tab_ui specific UI function, may take any number of arguments
    tab_ui = NULL,
    #' @field tab_ui_foot specific UI function to be used in footer (without arguments)
    tab_ui_foot = NULL,
    #' @field tab_ui specific server function
    tab_server = NULL,
    #' @field tab_number number assigned automatically based on 'ordered_tabs' vector of names
    tab_number = NULL,
    #' @field previous_tab name of previous tab (optional)
    previous_tab = NULL,
    #' @field previous_tab name of previous tab (optional)
    next_tab = NULL,
    #' @field id component id in shiny, assigned automatically based on tab number
    id = NULL,
    #' @field add_footer bool argument whether to include the standard footer
    add_footer = NULL,
    #' @field add_header bool argument whether to include the standard header
    add_header = NULL,
    #' @field exposure optional table of exposures, if present a grid of inputs will be prepared
    exposure = NULL,
    #' @field type type of institution (to group the inputs from all tabs)
    type = NULL,
    #' @field subtype subtype of tab within a type of institution (to group the inputs from all tabs)
    subtype = NULL,
    #' @field ui_pars optional list of parameter to pass to ui function
    ui_pars = NULL,
    #' @description the constructor fills the slots with values given
    #' it also automatically gets ui, server and foot
    #' from the relevant functions (or makes it empty if the function does not exist)
    initialize = function(tab_name, previous_tab, next_tab, add_header = TRUE, add_footer = TRUE,
                          exposure = NULL, type = NULL, subtype = NULL, ui_pars = list()) {
      self$tab_name <- tab_name
      self$tab_ui <- get_or_null(paste0("tab_", tab_name, "_ui"))
      self$tab_ui_foot <- get_or_null(paste0("tab_", tab_name, "_foot"))
      self$tab_server <- get_or_null(paste0("tab_", tab_name, "_server"))
      self$tab_number <- as.integer(factor(tab_name, ordered_tabs))
      self$previous_tab <- as.integer(factor(previous_tab, ordered_tabs))
      self$next_tab <- as.integer(factor(next_tab, ordered_tabs))
      self$id <- paste0("page_", self$tab_number)
      self$add_header <- add_header
      self$add_footer <- add_footer
      self$exposure <- exposure
      self$type <- type
      self$subtype <- subtype
      self$ui_pars <- ui_pars
    },
    #' @description Tab server function that combines:
    #' 1. server side of exposure input table (if given in the constructor)
    #' 2. any other server tab_server (if given in the constructor)
    #' 3. possibility of switch to previous/next tab (if applicable), using 'switch_page' function.
    #' additionally, a boolean function may be passed to allow going next only conditionally
    #' (by default the condition is always true)
    server = function(input, output, session, switch_page, allow_next=function(){TRUE}) {
      if (!is.null(self$exposure)) {
        if(ncol(self$exposure) < 5) {
          width <- '12em'
        } else {
          width <- '6em'
        }
        exposure_grid_server(
          input,
          output,
          self$exposure,
          produce_tooltip_matrix(self$exposure),
          paste(self$type, self$subtype, sep="_"),
          session$userData$dev,
          width
        )
      }
      if (!is.null(self$tab_server)) self$tab_server(input, output, session, self)
      if (length(self$previous_tab)) {
        observeEvent(
          input[[paste0(self$id, "_previous")]], switch_page(as.integer(self$previous_tab))
        )
      }
      if (length(self$next_tab)) {
        observeEvent(
          input[[paste0(self$id, "_next")]], 
          {
            if(allow_next()){
              switch_page(as.integer(self$next_tab))
            }
          }
        )
      }
    },
    #' @description tab UI function combines:
    #' 0. a common header (unless add_header=FALSE)
    #' 1. exposure input table (if exposure table given in the constructore)
    #' 2. any other tab_UI (if given in the constructor)
    #' 3. buttons that switch to previous/next tab (if applicable)
    #' 4. a tab-specific text
    #' 5. a common footer (unless add_footer=FALSE)
    ui = function() {
      tabpanel_params <- list(self$id)
      if (self$add_header) {
        tabpanel_params <- add_param(
          tabpanel_params,
          tag("header", list(
            img(src = "cfrf_logo.png", alt = "CFRF logo", height = 50)
          ))
        )
      }
      if (!is.null(self$tab_ui)) {
        tabpanel_params <- add_param(
          tabpanel_params, do.call(self$tab_ui, self$ui_pars)
        )
      }
      if (!is.null(self$exposure)) {
        tabpanel_params <- add_param(
          tabpanel_params,
          exposure_grid_ui(paste(self$type, self$subtype, sep="_"))
        )
      }
      tabpanel_params <- add_param(tabpanel_params, br())
      if (length(self$previous_tab)) {
        tabpanel_params <- add_param(
          tabpanel_params, actionButton(paste0(self$id, "_previous"), "prev")
        )
      }
      if (length(self$next_tab)) {
        tabpanel_params <- add_param(
          tabpanel_params, actionButton(paste0(self$id, "_next"), "next")
        )
        tabpanel_params <- add_param(
          tabpanel_params, textOutput(paste0(self$id, "_next_result"))
        )
      }
      if (!is.null(self$tab_ui_foot)) {
        tabpanel_params <- add_param(
          tabpanel_params, self$tab_ui_foot()
        )
      }

      if (self$add_footer) {
        tabpanel_params <- add_param(
          tabpanel_params, heartbeat_footer()
        )
      }
      do.call(tabPanel, tabpanel_params)
    }
  ),
)
