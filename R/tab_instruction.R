tab_instruction_ui <- function() {
  out <- list(
    uiOutput("instruction_text")
  )
  out
}

tab_instruction_server <- function(input, output, session) {
  include_markdown_section(output, "instruction_text", "instruction")
  if (global$dev == TRUE) {
    session$userData$prev_tabs[["instruction"]] <- tab_name_to_number("title")
  }
}
