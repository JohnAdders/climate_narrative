tab_intro_ui <- function() {
  out <- list(
    uiOutput("intro_text")
  )
  out
}

tab_intro_server <- function(input, output, session) {
  include_markdown_section(output, "intro_text", "instruction")
  if (global$dev == TRUE) {
    session$userData$prev_tabs[["intro"]] <- tab_name_to_number("title")
  }
}
