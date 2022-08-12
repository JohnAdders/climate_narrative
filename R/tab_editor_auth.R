tab_editor_auth_ui <- function() {
  if (!is.null(global$editor_code)) {
    return(
      column(
        12,
        p("Enter the code to edit the tool contents"),
        textInput(
          inputId = "editor_code",
          label = tagList(icon("unlock-alt"), "Verification code"),
          placeholder = "Enter your verification code"
        ),
        actionButton(
          paste0("page_", tab_name_to_number("auth"), "_previous"),
          "prev"
        ),
        actionButton(
          inputId = "button_check_editor_code",
          label = "Validate the code"
        ),
        tippy::tippy_this("button_check_editor_code", "Click to proceed (if the code is correct)"),
        textOutput("editor_code_verification_result")
      )
    )
  } else {
    return(
      column(
        12,
        p("No authorization required, please click next to proceed"),
        actionButton(
          paste0("page_", tab_name_to_number("editor_auth"), "_previous"),
          "prev"
        ),
        actionButton(
          inputId = "button_check_editor_code",
          label = "Next"
        ),
      )
    )
  }
}

tab_editor_auth_server <- function(input, output, session) {
  observeEvent(input$button_check_editor_code, {
    if (!is.null(global$editor_code)) {
      process_progress_editor(output, input$editor_code == global$editor_code)
    } else {
      process_progress_editor(output, TRUE)
    }
  })
}

process_progress_editor <- function(output, should_continue) {
  if (should_continue) {
    next_tab <- tab_name_to_number("editor")
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", next_tab))
  } else {
    output$editor_code_verification_result <- renderText("Code incorrect, please double check")
  }
}
