passes_captcha <- function(input, session) {
  result <- GreCAPTCHAv3Server(session$userData$captcha_secret, input$responseReceived)
  return(result$success && result$score > 0.5)
}

request_captcha <- function(output, session) {
  if (!is.null(session$userData$captcha_code) && !is.null(session$userData$captcha_secret)) {
    GreCAPTCHAv3js(session$userData$captcha_code, "homepage", "responseReceived")
  } else {
    output$code_send_result <- renderText("Captcha configuration missing, can't proceed")
  }
}

process_progress <- function(output, should_continue) {
  if (should_continue) {
    next_tab <- as.integer(factor("type", ordered_tabs))
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", next_tab))
  } else {
    output$code_verification_result <- renderText("Code incorrect, please double check")
  }
}

render_dynamic_auth_ui <- function(output, session) {
  if (!is.null(session$userData$beta_code)) {
    output$first_column <- renderUI({NULL})
    output$auth_text <- renderUI({p("Enter the beta code you have been sent")})
  } else {
    output$first_column <- renderUI(
      {
        column(
          6,
          p("Enter your email address to receive the verification code"),
          textInput(
            inputId = "email",
            label = tagList(icon("user"), "Email"),
            placeholder = "Enter your email here"
          ),
          actionButton(
            inputId = "button_send_code",
            label = "Send the code"
          ),
          tippy_this("button_send_code", "Delivering the email may take several minutes, please also check your spam folder"),
          textOutput("code_send_result")
        )
      }
    )
    output$auth_text <- renderUI({p("Enter the verification code received in your email")})
  }
}

send_auth_code_email <- function(input, output, session) {
  output$code_send_result <- renderText(
    paste(
      "TODO: send the actual email from",
      session$userData$email_server,
      "to",
      input$email,
      "containing the code:",
      session$userData$verification_code
    )
  )
}

tab_auth_ui <- function() {
  list(
    div(
      class = "disclaimer", id = "disclaimer_1",
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
    fluidRow(
      uiOutput("first_column"),
      column(
        6,
        uiOutput("auth_text"),
        textInput(
          inputId = "code",
          label = tagList(icon("unlock-alt"), "Verification code"),
          placeholder = "Enter your verification code"
        ),
        actionButton(
          inputId = "button_check_code",
          label = "Validate the code"
        ),
        tippy_this("button_check_code", 'Click to proceed (if the code is correct)'),
        textOutput("code_verification_result")
      )
    )
  )
}

tab_auth_server <- function(input, output, session, tab) {
  render_dynamic_auth_ui(output, session)
  observeEvent(
    input$button_send_code,
    {
      if (grepl("@", input$email, fixed = TRUE)) {
        request_captcha(output, session)
      } else {
        output$code_send_result <- renderText("Please provide a valid email")
      }
    }
  )

  observeEvent(
    input$responseReceived,
    {
      if(session$userData$captcha_validated == FALSE) {
        session$userData$captcha_validated <- passes_captcha(input, session)
      }
      if(session$userData$captcha_validated == TRUE) {
        if(!is.null(session$userData$beta_code)) {
          process_progress(output, input$code == session$userData$beta_code)
        } else {
          send_auth_code_email(input, output, session)
        }
      }
    }
  )

  observeEvent(
    input$button_check_code,
    {
      if(!is.null(session$userData$beta_code)) {
        request_captcha(output, session)
      } else {
        process_progress(
          output,
          (
            input$code == session$userData$verification_code &&
            session$userData$captcha_validated == TRUE
          )
        )
      }
    }
  )
}
