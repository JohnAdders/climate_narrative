tab_auth_ui <- function () {
  list(
    p("Are you a human being?"),
    GreCAPTCHAv3Ui("6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP",'homepage','responseReceived'),
    textInput(
      inputId='secret',
      label='recaptcha v3 secret code',
      placeholder='temporary solution'
    ),
    actionButton(
      inputId='captcha',
      label='Yes, I am a human'
    ),
    textOutput('captcha_validation_result'),
    hr(),
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

tab_auth_server <- function (input, output, session, tab) {
  observeEvent(
    input$captcha,
    {
      # runjs('https://www.google.com/recaptcha/api.js?render=6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP')
      # runjs(paste0("
      #   grecaptcha.ready(function () {
      #     grecaptcha.execute('6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP', { action: 'homepage' }).then(function (token) {
			#       Shiny.onInputChange('responseReceived',token);
      # 		});
	    #   });
      # "))
      GreCAPTCHAv3js('6LfQwf8cAAAAAGsbrln3KpFJ69IoSdZPaCGLiUzP')
    }
  )

observeEvent(input$responseReceived, {
    result <- GreCAPTCHAv3Server(input$secret, input$responseReceived)
    if(result$success){
      #info(result)
      session$userData$captcha_validated = TRUE
      output$captcha_validation_result = renderText(paste(result,collapse='.'))#'OK')
    } else {
      output$captcha_validation_result = renderText(paste(result,collapse='.'))#'Try once again')
    }
  })

  observeEvent(
    input$send_code,
    if(session$userData$captcha_validated) {
      output$verification_code <- renderText(session$userData$verification_code)
    } else {
      output$verification_code <- renderText("Prove that you are a human first")
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