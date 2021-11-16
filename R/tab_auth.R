tab_auth_ui <- function () {
  list(
    div(class='disclaimer',
      p("This tool and its contents represent the output from the cross-industry
      Scenario Analysis Working Group of the Prudential Regulation Authority
      and Financial Conduct Authority's Climate Financial Risk Forum (CFRF).
      The tool aims to promote understanding, consistency, and comparability
      by providing guidance on how to use scenario analysis to assess financial impacts
      and inform strategy/business decisions."),
      p("This tool has been written by industry, for industry. The recommendations in this tool
      do not constitute financial or other professional advice and should not be relied upon as such.
      The PRA and FCA have convened and facilitated CFRF discussions but do not accept liability
      for the views expressed in this tool which do not necessarily represent the view
      of the regulators and in any case do not constitute regulatory guidance."),
      p(strong("Copyright 2021 The Climate Financial Risk Forum"))
    ),
    hr(),
    GreCAPTCHAv3Ui("6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP"),
    fluidRow(column(6,
      p('Enter your email address to receive the verification code'),
      textInput(
        inputId='email',
        label=tagList(icon("user"),"Email"),
        placeholder = 'Enter your email here'
      ),
      actionButton(
        inputId='button_send_code',
        label='Send the code'
      ),
      textOutput('captcha_verification_result'),
      textOutput('code_send_result')
    ),
    column(6,
      p('Enter the verification code received in your email'),
      textInput(
        inputId='code',
        label=tagList(icon("unlock-alt"), "Verification code") ,
        placeholder = 'Enter your verification code'
      ),
      actionButton(
        inputId='button_check_code',
        label='Validate the code'
      ),
      textOutput('code_verification_result')
    ))
  )
}

tab_auth_server <- function (input, output, session, tab) {
  
  observeEvent(input$button_send_code, {
    if(grepl('@', input$email, fixed=TRUE)){
      output$captcha_verification_result = renderText('Captcha request sent to uncle google...')
      if(!is.null(session$userData$captcha_code)){
        GreCAPTCHAv3js('6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP', 'homepage', 'responseReceived')
      } else {
        session$userData$captcha_validated = TRUE
        output$captcha_verification_result = renderText('Development mode. No captcha required')
        output$code_send_result <- renderText(paste('Development mode, the code is: ', session$userData$verification_code))
      }
    } else {
      output$code_send_result <- renderText('Please provide a valid email')
    }
  })

observeEvent(input$responseReceived, {    
      result <- GreCAPTCHAv3Server(session$userData$captcha_code, input$responseReceived)
      if(result$success){
        session$userData$captcha_validated = TRUE
        output$captcha_verification_result = renderText(paste('OK. Full validation result: ', paste(result, collapse=';')))
        output$code_send_result <- renderText(paste(
          'TODO: send the actual email from',
          session$userData$email_server,
          'to',
          input$email,
          'containing the code:',
          session$userData$verification_code
        ))
      } else {
        output$captcha_verification_result = renderText(paste('Try once again. Full validation result: ', paste(result, collapse=';')))
      }
  })

  observeEvent(
    input$button_check_code,#input$code,
    {
      if (input$code == session$userData$verification_code | session$userDat$dev ==TRUE) {
        tab$next_tab <- as.integer(factor('type', ordered_tabs))
        output$code_verification_result <- renderText('Code correct, please proceed')
      } else {
        tab$next_tab <- as.integer(factor('auth', ordered_tabs))
        output$code_verification_result <- renderText('Code incorrect, please double check')
      }
    }
  )
}