tab_editor_helper <- function() {
  helpText(
    list(
      "This tab allows to edit the content (descriptions for each sector/asset class). Please note that: ",
      tags$ul(
        tags$li(
          paste0(
            "The input is designed to be as user-friendly as possible, using a third party package for WYSIWYG ",
            "(what-you-see-is-what-you-get) markdown editor. ",
            "However, this third party package is likely a work-in-progress and may not always behave as expected."
          )
        ),
        tags$li(
          paste0(
            "It is possible to embed graphs into the text. However, it is recommended to add new graphs by contacting ",
            "the support (link can be found at the bottom of the webpage). ",
            "We are also happy to help modify/delete the existing files as required."
          )
        ),
        tags$li(
          paste0(
            "At this point, the preview window may not render images properly. ",
            "Again, when in doubt it is advised to contact the support using the link at the bottom of the page."
          )
        ),
        tags$li(
          paste0(
            "The editor does not handle properly particular sovereigns update at the moment! "
          )
        )
      )
    )
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
      uiOutput("input_field")
    ),
    column(
      6,
      actionButton("update_preview", "Update preview"),
      helpText("Click this to see below how this section will look like (subject to the limitations described above)"),
      actionButton("save", "Save changes"),
      helpText("This button saves the changes to the selected sector"),
      h4("Preview"),
      uiOutput("edited")
    )
  )
}

#' @importFrom shinymarkdown mdInput
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
      } else {
        exposure_class_no <- which(lapply(global$exposure_classes, function(x) x$name) == input$editor_sector_selection)
        exposure_class <- global$exposure_classes[[exposure_class_no]]
        if (input$editor_section_selection %in% c("description", "references")) {
          subsection_options <- c("N/A")
        } else {
          subsection_options <- c("exec_description", "always", "high_materiality")
        }
      }
      updateSelectInput(
        session = session,
        inputId = "editor_subsection_selection",
        choices = subsection_options,
      )
    }
  )

  default_editor_value <- reactive({
    if (input$editor_section_selection == "" || input$editor_subsection_selection == "") {
      editor_value <- ""
    } else {
      exposure_class_no <- which(lapply(global$exposure_classes, function(x) x$name) == input$editor_sector_selection)
      if (length(exposure_class_no)) {
        exposure_class <- global$exposure_classes[[exposure_class_no]]
        risk_and_materiality <- strsplit(input$editor_section_selection, " / ")[[1]]
        if (length(risk_and_materiality) == 2) {
          risk <- risk_and_materiality[1]
          materiality <- risk_and_materiality[2]
          editor_value <- exposure_class[[risk]][[materiality]][[input$editor_subsection_selection]]
        } else {
          editor_value <- exposure_class[[input$editor_section_selection]]
        }
      } else {
        editor_value <- ""
      }
    }
    # remove target="_blank", they are reinserted later on in include_markdown_text anyway
    editor_value <- gsub('{target="\\_blank"}', "", editor_value, fixed = TRUE)
    return(editor_value)
  })

  observeEvent(
    input$editor_subsection_selection,
    {
      output$input_field <- renderUI({
        mdInput(
          inputId = "editor",
          hide_mode_switch = F,
          initial_value = default_editor_value(),
          initial_edit_type = "wysiwyg"
        )
      })
      include_markdown_text(default_editor_value(), output, "edited", TRUE)
    }
  )
  observeEvent(
    input$update_preview,
    {
      markdown_text <- input$editor_markdown
      if (is.null(markdown_text)) {
        markdown_text <- default_editor_value()
      }
      include_markdown_text(markdown_text, output, "edited", TRUE)
    }
  )
  observeEvent(
    input$save,
    {
      if (input$editor_sector_selection != "") {
        # for a given sector name, find the respective file name
        exposure_files <- dir(system.file("exposure_class", package = "climate.narrative"))
        exposure_names <- character(length(exposure_files))
        exposure_pretty_names <- character(length(exposure_files))
        for (i in 1:length(exposure_files)) {
          exposure_names[i] <- remove_special_characters(substr(exposure_files[i], 1, nchar(exposure_files[i]) - 4))
          exposure_pretty_names[i] <- global$exposure_classes[[exposure_names[i]]]$name
        }
        index <- which(exposure_pretty_names == input$editor_sector_selection)
        if (length(index) != 1) {
          stop("Error in saving the yml file, unique matching name not found")
        }
        section_subsection <- unlist(strsplit(input$editor_section_selection, " / "))
        if (input$editor_subsection_selection != "N/A") {
          section_subsection <- c(section_subsection, input$editor_subsection_selection)
        }
        markdown_text <- input$editor_markdown
        if (is.null(markdown_text)) {
          markdown_text <- default_editor_value()
        }
        # TODO: what about updating sovereigns?
        replace_yaml_subsection(
          paste0(system.file("exposure_class", package = "climate.narrative"), "/", exposure_files[index]),
          section_subsection,
          markdown_text
        )
        # Notify the user
        showModal(
          modalDialog(
            paste0(
              "Changes to the ",
              exposure_pretty_names[index],
              " exposure content file saved!"
            ),
            title = "Success",
            footer = modalButton("OK")
          )
        )
        # Update global as well
        # Note: impacts all users on the server
        global$exposure_classes <- read_dir("exposure_class")
      }
    }
  )
}
