tab_editor_helper <- function() {
  helpText(
    "This tab allows to edit the content"
  )
}

tab_editor_ui <- function() {
  scenario_options <- c(
    "",
    unname(unlist(lapply(global$exposure_classes, function(x) x$name)))
  )
  fluidRow(
    column(
      6,
      selectInput(
        "editor_sector_selection",
        "Select the sector to edit",
        scenario_options,
        selectize = FALSE
      ),
      selectInput(
        "editor_section_selection",
        "Select the section to edit",
        c(""),
        selectize = FALSE
      ),
      selectInput(
        "editor_subsection_selection",
        "Select the particular subsection to edit",
        c(""),
        selectize = FALSE
      ),
      textAreaInput(
        "editor",
        "Raw input",
        width = "90%",
        rows = 15
      )
    ),
    column(
      6,
      h4("Preview"),
      uiOutput("edited")
    )
  )
}

tab_editor_server <- function(input, output, session) {
  observeEvent(
    input$editor_sector_selection,
    {
      if (input$editor_sector_selection != "") {
        section_options <- c(
          "",
          "description",
          "transition / high",
          "transition / low",
          "physical / high",
          "physical / low",
          "references"
        )
      } else {
        section_options <- c("")
      }
      updateSelectInput(
        session = session,
        inputId = "editor_section_selection",
        choices = section_options,
      )
    }
  )
  observeEvent(
    input$editor_section_selection,
    {
      if (input$editor_section_selection == "") {
        subsection_options <- c("")
        editor_value <- ""
      } else {
        exposure_class_no <- which(lapply(global$exposure_classes, function(x) x$name) == input$editor_sector_selection)
        exposure_class <- global$exposure_classes[[exposure_class_no]]
        if (input$editor_section_selection %in% c("description", "references")) {
          subsection_options <- c("N/A")
          editor_value <- exposure_class[[input$editor_section_selection]]
        } else {
          subsection_options <- c("exec_description", "always", "high_materiality")
          editor_value <- ""
        }
      }
      updateSelectInput(
        session = session,
        inputId = "editor_subsection_selection",
        choices = subsection_options,
      )
    }
  )
  observeEvent(
    input$editor_subsection_selection,
    {
      if (input$editor_section_selection == "") {
        editor_value <- ""
        edited_html <- ""
      } else {
        exposure_class_no <- which(lapply(global$exposure_classes, function(x) x$name) == input$editor_sector_selection)
        exposure_class <- global$exposure_classes[[exposure_class_no]]
        if (input$editor_subsection_selection == "N/A") {
          editor_value <- exposure_class[[input$editor_section_selection]]
        } else {
          risk_and_materiality <- strsplit(input$editor_section_selection, " / ")[[1]]
          risk <- risk_and_materiality[1]
          materiality <- risk_and_materiality[2]
          editor_value <- exposure_class[[risk]][[materiality]][[input$editor_subsection_selection]]
        }
      }
      updateTextAreaInput(
        session = session,
        inputId = "editor",
        value = editor_value,
      )
      # remove target="_blank" because include_markdown_text does not handle it correctly
      markdown_text <- gsub('{target="\\_blank"}', "", editor_value, fixed = TRUE)
      include_markdown_text(markdown_text, output, "edited", FALSE)
    }
  )
}
