passes_captcha <- function(input, session) {
  result <- GreCAPTCHAv3Server(userData$captcha_secret, input$responseReceived)
  return(result$success && result$score > 0.5)
}

request_captcha <- function(output, session) {
  if (!is.null(userData$captcha_code) && !is.null(userData$captcha_secret)) {
    GreCAPTCHAv3js(userData$captcha_code, "homepage", "responseReceived")
  } else {
    output$code_send_result <- renderText("Captcha configuration missing, can't proceed")
  }
}

process_progress <- function(output, should_continue) {
  if (should_continue) {
    next_tab <- as.integer(factor("type", global$ordered_tabs))
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", next_tab))
  } else {
    output$code_verification_result <- renderText("Code incorrect, please double check")
  }
}

render_dynamic_auth_ui <- function(output, session) {
  if (!is.null(userData$beta_code)) {
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
          tippy::tippy_this("button_send_code", "Delivering the email may take several minutes, please also check your spam folder"),
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
      userData$email_server,
      "to",
      input$email,
      "containing the code:",
      userData$verification_code
    )
  )
}

tab_auth_ui <- function(captcha_code) {
  list(
    div(
      class = "disclaimer", id = "disclaimer_1",
      p("This tool and its contents represent the output from the cross-industry
      Scenario Analysis Working Group of the Prudential Regulation Authority
      and Financial Conduct Authority's Climate Financial Risk Forum (CFRF).
      The tool aims to promote understanding, consistency, and comparability
      by providing guidance on how to use scenario analysis to assess financial impacts
      and inform strategy/business decisions."),
      p("The tool has not been endorsed or approved by the PRA, FCA, the Climate Financial Risk Forum
      or any of its members, and no reliance should be placed on the accuracy or
      completeness of any information herein."),
      p("The software is provided \"as is\" without warranty of any kind, express or
      implied, including but not limited to the warranties or merchantability, fitness
      for a particular purpose and noninfringement.  In no event shall the authors
      or copyright holders be liable for any claim, damages or other liability,
      whether in an action of contract, tort or otherwise arising from, out of or in
      connection with the software or the use or other dealings in the software."),
      p(strong("Copyright 2021 The Climate Financial Risk Forum"))
    ),
    hr(),
    GreCAPTCHAv3Ui(captcha_code),
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
        tippy::tippy_this("button_check_code", 'Click to proceed (if the code is correct)'),
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
        if(!is.null(userData$beta_code)) {
          process_progress(output, input$code == userData$beta_code)
        } else {
          send_auth_code_email(input, output, session)
        }
      }
    }
  )

  observeEvent(
    input$button_check_code,
    {
      if(!is.null(userData$beta_code)) {
        request_captcha(output, session)
      } else {
        process_progress(
          output,
          (
            input$code == userData$verification_code &&
            session$userData$captcha_validated == TRUE
          )
        )
      }
    }
  )
}
