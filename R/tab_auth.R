tab_auth_ui <- function () {
  list(
    p("Are you a human being?"),
    GreCAPTCHAv3Ui("6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP"),
    actionButton(
      inputId='button_captcha',
      label='Yes, I am a human'
    ),
    textOutput('captcha_verification_result'),
    hr(),
    p('Enter your email address to receive the verification code'),
    textInput(
      inputId='email',
      label=tagList(icon("user"),"Email"),
      placeholder = 'Enter your email here'
    ),
    actionButton(
      inputId='button_send_code',
      label='Send the code '
    ),
    hr(),
    textOutput('code_verification_result'),
    p('Enter the verification code received in your email'),
    textInput(
      inputId='code',
      label=tagList(icon("unlock-alt"), "Verification code") ,
      placeholder = 'Enter your verification code'
    ),
    hr()
  )
}

tab_auth_server <- function (input, output, session, tab) {
  observeEvent(input$button_captcha, {
    output$captcha_verification_result = renderText('Captcha request sent to uncle google...')
    if(!is.null(session$userData$captcha_code)){
      GreCAPTCHAv3js('6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP', 'homepage', 'responseReceived')
    } else {
      session$userData$captcha_validated = TRUE
      output$captcha_verification_result = renderText('Development mode. No captcha required')
    }
  })

observeEvent(input$responseReceived, {    
    result <- GreCAPTCHAv3Server(session$userData$captcha_code, input$responseReceived)
    if(result$success){
      session$userData$captcha_validated = TRUE
      output$captcha_verification_result = renderText(paste('OK. Full validation result: ', paste(result, collapse=';')))
    } else {
      output$captcha_verification_result = renderText(paste('Try once again. Full validation result: ', paste(result, collapse=';')))
    }
  })

  observeEvent(input$button_send_code, {
    if(session$userData$captcha_validated) {
      if(is.null(session$userData$email_server)){
        output$code_verification_result <- renderText(paste('Development mode, the code is: ', session$userData$verification_code))
      } else {
        if(grepl('@', input$email, fixed=TRUE)){
          output$code_verification_result <- renderText(paste(
            'TODO: send the actual email from',
            session$userData$email_server,
            'to',
            input$email,
            'containing the code:',
            session$userData$verification_code
          ))
        } else {
          output$code_verification_result <- renderText('Please provide a valid email')
        }
      }
    } else {
      output$code_verification_result <- renderText("Prove that you are a human first")
    }
  })

  observeEvent(
    input$code,
    {
      if (input$code == session$userData$verification_code) {
        tab$next_tab <- 2
      } else {
        tab$next_tab <- 2#1
      }
    }
  )
}