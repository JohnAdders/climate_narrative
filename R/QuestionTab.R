#' QuestionTab class definition
#'
#' Instances are tabs of the questionnaire.
#' This is a wrapper over any ui and server
#' (if the initial_next_tab/initial_previous_tab argument is NULL there is no button and corresponding server logic).
QuestionTab <- R6::R6Class(
  "QuestionTab",
  public = list(
    #' @field tab_name name of the tab
    tab_name = NULL,
    #' @field tab_title pretty title to be used in the UI
    tab_title = NULL,
    #' @field tab_ui specific UI function, may take any number of arguments
    tab_ui = NULL,
    #' @field tab_ui_helper specific UI function to be used in helper (without arguments)
    tab_ui_helper = NULL,
    #' @field tab_server specific server function
    tab_server = NULL,
    #' @field tab_number number assigned automatically based on 'ordered_tabs' vector of names
    tab_number = NULL,
    #' @field initial_previous_tab name of previous tab (optional)
    initial_previous_tab = NULL,
    #' @field initial_next_tab name of previous tab (optional)
    initial_next_tab = NULL,
    #' @field id component id in shiny, assigned automatically based on tab number
    id = NULL,
    #' @field add_footer bool argument whether to include the standard footer
    add_footer = NULL,
    #' @field add_header bool argument whether to include the standard header
    add_header = NULL,
    #' @field add_buttons bool argument whether to include the standard prev/next buttons
    add_buttons = NULL,
    #' @field exposure optional table of exposures, if present a grid of inputs will be prepared
    exposure = NULL,
    #' @field type type of institution (to group the inputs from all tabs)
    type = NULL,
    #' @field subtype subtype of tab within a type of institution (to group the inputs from all tabs)
    subtype = NULL,
    #' @field ui_settings optional list of parameters to pass to ui function
    ui_settings = NULL,
    #' @field bottom_offset move the buttons and footer to the right by this fraction of page
    bottom_offset = NULL,
    #' @description the constructor fills the slots with values given
    #' it also automatically gets ui, server and foot
    #' from the relevant functions (or makes it empty if the function does not exist)
    #' @param tab_name name of the tab
    #' @param tab_title title of the tab to be used in the UI
    #' @param initial_previous_tab name of the previous tab
    #' @param initial_next_tab name of the next tab
    #' @param add_header whether to add a standard header
    #' @param add_footer whether to add a standard footer
    #' @param add_buttons whether to add a standard prev/next buttons
    #' @param exposure (optional) table of exposure to construct grid of inputs
    #' @param type type of issuers for which the inputs are applicable
    #' @param subtype unique reference of tab within a type of issuers
    #' @param ui_settings (optional) list of arguments to pass to tab_ui
    #' @param bottom_offset (optional) move the buttons and footer to the right by this fraction of page
    initialize = function(tab_name, tab_title, initial_previous_tab, initial_next_tab, add_header = TRUE,
                          add_footer = TRUE, add_buttons = TRUE, exposure = NULL, type = NULL, subtype = NULL,
                          ui_settings = list(), bottom_offset = 0) {
      self$tab_name <- tab_name
      self$tab_title <- tab_title
      self$tab_ui <- get0(paste0("tab_", tab_name, "_ui"))
      self$tab_ui_helper <- get0(paste0("tab_", tab_name, "_helper"))
      self$tab_server <- get0(paste0("tab_", tab_name, "_server"))
      self$tab_number <- tab_name_to_number(tab_name)
      self$initial_previous_tab <- tab_name_to_number(initial_previous_tab)
      self$initial_next_tab <- tab_name_to_number(initial_next_tab)
      self$id <- paste0("page_", self$tab_number)
      self$add_header <- add_header
      self$add_footer <- add_footer
      self$add_buttons <- add_buttons
      self$exposure <- exposure
      self$type <- type
      self$subtype <- subtype
      self$ui_settings <- ui_settings
      self$bottom_offset <- bottom_offset
    },
    #' @description Tab server function that combines:
    #' 1. server side of exposure input table (if given in the constructor)
    #' 2. any other server tab_server (if given in the constructor)
    #' additionally, a boolean function may be passed to allow going next only conditionally
    #' 3. possibility of switch to previous/next tab (if applicable), using 'switch_page' function.
    #'
    #' @param input regular shiny parameter
    #' @param output regular shiny parameter
    #' @param session regular shiny parameter
    #' @param switch_page function to be passed that changes the active tab
    #' (used in prev/next buttons)
    server = function(input, output, session, switch_page) {
      if (!is.null(self$exposure)) {
        if (ncol(self$exposure) < 5) {
          width <- "12em"
        } else {
          width <- "6em"
        }
        exposure_grid_server(
          input,
          output,
          self$exposure,
          paste(self$type, self$subtype, sep = "_"),
          global$dev,
          width
        )
        # in order to create a full report without visiting the tabs (e.g. dev mode or sector report):
        outputOptions(output, paste(self$type, self$subtype, sep = "_"), suspendWhenHidden = FALSE)
      }
      if (!is.null(self$tab_server)) self$tab_server(input, output, session)
      if (length(self$initial_previous_tab)) {
        observeEvent(
          input[[paste0(self$id, "_previous")]],
          switch_page(session$userData$prev_tabs[[self$tab_name]])
        )
      }
      if (length(self$initial_next_tab)) {
        observeEvent(
          input[[paste0(self$id, "_next")]],
          {
            switch_page(session$userData$next_tabs[[self$tab_name]])
          }
        )
      }
    },
    #' @description tab UI function that combines:
    #' 1. a common header (unless add_header=FALSE)
    #' 2. title (unless tab_title is NULL)
    #' 3. exposure input table (if exposure table given in the constructore)
    #' 4. any other tab_UI (if given in the constructor)
    #' 5. buttons that switch to previous/next tab (if applicable)
    #' 6. a tab-specific text
    #' 7. a common footer (unless add_footer=FALSE)
    ui = function() {
      tabpanel_params <- list(self$id)
      if (self$add_header) {
        tabpanel_params <- add_param(
          tabpanel_params,
          tags$header(
            list(
              img(src = "climate_narrative/cfrf_logo.png", alt = "CFRF logo", height = 50)
            )
          )
        )
      }
      if (!is.null(self$tab_title)) {
        tabpanel_params <- add_param(
          tabpanel_params,
          h2(self$tab_title)
        )
      }
      if (!is.null(self$tab_ui_helper)) {
        tabpanel_params <- add_param(
          tabpanel_params, self$tab_ui_helper()
        )
      }
      if (!is.null(self$tab_ui)) {
        tabpanel_params <- add_param(
          tabpanel_params, do.call(self$tab_ui, self$ui_settings)
        )
      }
      if (!is.null(self$exposure)) {
        tabpanel_params <- add_param(
          tabpanel_params,
          exposure_grid_ui(paste(self$type, self$subtype, sep = "_"))
        )
      }
      tabpanel_params <- add_param(tabpanel_params, br())
      ui_bottom <- list()
      if (length(self$initial_previous_tab) & self$add_buttons) {
        ui_bottom <- add_param(
          ui_bottom, actionButton(paste0(self$id, "_previous"), "prev")
        )
      }
      if (length(self$initial_next_tab) & self$add_buttons) {
        ui_bottom <- add_param(
          ui_bottom, actionButton(paste0(self$id, "_next"), "next")
        )
        ui_bottom <- add_param(
          ui_bottom, textOutput(paste0(self$id, "_next_result"))
        )
      }
      if (self$add_footer) {
        ui_bottom <- add_param(
          ui_bottom, heartbeat_footer()
        )
      }
      bottom_offset_int <- floor(self$bottom_offset * 12)
      tabpanel_params <- add_param(
        tabpanel_params,
        fluidRow(
          column(12 - bottom_offset_int, ui_bottom, offset = bottom_offset_int)
        )
      )
      do.call(tabPanel, tabpanel_params)
    }
  ),
)
