tab0_ui <- function () {
  list(
    p('Enter your email address to receive the verification code'),
    p('(does nothing at the moment)'),
    textInput(
      inputId='email',
      label=tagList(icon("user"),"Email"),
      placeholder = 'Enter your email here'
    ),
    actionButton(
      inputId='send_code',
      label='Send the code '
    ),
    hr(),
    p('Enter the verification code received in your email'),
    textOutput("verification_code"),
    textInput(
      inputId='show_app',
      label=tagList(icon("unlock-alt"), "Verification code") ,
      placeholder = 'Enter your verification code'
    ),
    hr()
  )
}

tab0_server <- function (input, output, session, tab) {
  observeEvent(
    input$send_code,
    {
      output$verification_code <- renderText(session$userData$verification_code)
    }
  )

  observeEvent(
    input$show_app,
    {
      if (input$show_app == session$userData$verification_code) {
        tab$next_tab <- 2
      } else {
        tab$next_tab <- 1
      }
    }
  )
}