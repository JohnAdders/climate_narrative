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
    exposure = NULL,
    type = NULL,
    subtype = NULL,
    initialize = function(tab_name, previous_tab, next_tab, add_header = TRUE, add_footer = TRUE,
                          exposure = NULL, type = NULL, subtype = NULL) {
      # the constructor automatically gets ui, server and foot
      # from the relevant functions (or makes it empty if the function does not exist)
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
    },
    # tab server function that combines:
    # 1. server side of exposure input table (if given in the constructor)
    # 2. any other server tab_server (if given in the constructor)
    # 3. possibility of switch to previous/next tab (if applicable), using 'switch_page' function.
    server = function(input, output, session, switch_page, allow_next) {
      switch_page <- function(i) updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
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
    # tab UI function combines:
    # 0. a common header (unless add_header=FALSE)
    # 1. exposure input table (if exposure table given in the constructore)
    # 2. any other tab_UI (if given in the constructor)
    # 3. buttons that switch to previous/next tab (if applicable)
    # 4. a tab-specific text
    # 5. a common footer (unless add_footer=FALSE)
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
          tabpanel_params, self$tab_ui()
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
