tab_editor_helper <- function() {
  helpText(
    list(
      "This tab allows to edit the content (descriptions for each sector/asset class). Please note that: " ,
      tags$ul(
        tags$li(
          paste0(
            "The user is not very user friendly, to get a specific formatting you need to use markdown syntax. ",
            "For example, to get a text in bold please surround it by double asterisks like this: **text**"
          )
        ),
        tags$li(
          paste0(
            "Currently there is no possibility to upload files (e.g. graphs). If you need to upload a new file ",
            "(or modify/delete the existing one), please contact the support ",
            "(link can be found at the bottom of the webpage)"
          )
        ),
        tags$li(
          paste0(
            "The preview window will also not render images properly, even if those are uploaded to the server. "
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
      textAreaInput(
        "editor",
        "Raw input",
        width = "90%",
        rows = 15
      )
    ),
    column(
      6,
      actionButton("update_preview", "Update preview"),
      helpText("Click this to see below how this section will look like (subject to the limitation listed above)"),
      actionButton("save", "Save changes"),
      helpText("This button saves the changes to the selected sector"),
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
  observeEvent(
    input$update_preview,
    {
      markdown_text <- gsub('{target="\\_blank"}', "", input$editor, fixed = TRUE)
      include_markdown_text(markdown_text, output, "edited", FALSE)
    }
  )
  observeEvent(
    input$save,
    {
      if (input$editor_sector_selection != ""){
        # for a given sector name, find the respective file name
        exposure_files <- dir(system.file("exposure_class", package = "climate.narrative"))
        exposure_names <- character(length(exposure_files))
        exposure_pretty_names <- character(length(exposure_files))
        for (i in 1:length(exposure_files)){
          exposure_names[i] <- remove_special_characters(substr(exposure_files[i], 1, nchar(exposure_files[i])-4))
          exposure_pretty_names[i] <- global$exposure_classes[[exposure_names[i]]]$name
        }
        index <- which(exposure_pretty_names == input$editor_sector_selection)
        if (length(index) != 1){
          stop("Error in saving the yml file")
        }
        section_subsection <- unlist(strsplit(input$editor_section_selection, " / "))
        if (input$editor_subsection_selection != "N/A") {
          section_subsection <- c(section_subsection, input$editor_subsection_selection)
        }
        replace_yaml_subsection(
          paste0(system.file("exposure_class", package = "climate.narrative"), "/", exposure_files[index]),
          section_subsection,
          input$editor
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
