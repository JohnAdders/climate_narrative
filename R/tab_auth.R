passes_captcha <- function(input, session) {
  result <- recaptcha_server(global$captcha_secret, input$responseReceived)
  print(
    paste0(
      "Captcha attempt. Details: success ",
      result$success,
      " | score ",
      result$score,
      " | hostname ",
      result$hostname
    )
  )
  return(
    any(
      sapply(as.list(global$ip_whitelist), function(x) startsWith(result$hostname, x)),
      (result$success && result$score > global$captcha_threshold)
    )
  )
}

request_captcha <- function(output, session) {
  if (global$dev == FALSE) {
    if (!is.null(global$captcha_code) && !is.null(global$captcha_secret)) {
      recaptcha_js(global$captcha_code, "homepage", "responseReceived")
    } else {
      output$code_send_result <- renderText("Captcha configuration missing, can't proceed")
    }
  } else {
    next_tab <- tab_name_to_number("instruction")
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", next_tab))
  }
}

process_progress <- function(output, should_continue) {
  if (should_continue) {
    next_tab <- tab_name_to_number("instruction")
    updateTabsetPanel(inputId = "wizard", selected = paste0("page_", next_tab))
  } else {
    output$code_verification_result <- renderText("Code incorrect, please double check")
  }
}

render_dynamic_auth_ui <- function(output, session) {
  if (!is.null(global$beta_code)) {
    output$first_column <- renderUI({
      column(
        12,
        p("Enter the beta code you have been sent"),
        textInput(
          inputId = "code",
          label = tagList(icon("unlock-alt"), "Verification code"),
          placeholder = "Enter your verification code"
        ),
        actionButton(
          inputId = "button_check_code",
          label = "Validate the code"
        ),
        textOutput("code_verification_result")
      )
    })
  } else {
    output$first_column <- renderUI({
      column(
        12,
        actionButton(
          paste0("page_", tab_name_to_number("auth"), "_previous"),
          "prev"
        ),
        actionButton(
          inputId = "button_check_code",
          label = "Next"
        ),
        textOutput("code_verification_result")
      )
    })
  }
}


tab_auth_ui <- function(captcha_code) {
  list(
    div(
      class = "disclaimer", id = "disclaimer_1",
      uiOutput("disclaimer_text")
    ),
    hr(),
    recaptcha_ui(captcha_code),
    fluidRow(
      uiOutput("first_column")
    )
  )
}

tab_auth_server <- function(input, output, session) {
  include_markdown_section(output, "disclaimer_text", "disclaimer")

  render_dynamic_auth_ui(output, session)

  observeEvent(
    input$responseReceived,
    {
      if (session$userData$captcha_validated == FALSE) {
        captcha_result <- passes_captcha(input, session)
        if (captcha_result == FALSE) {
          warning("Captcha verification failed")
          output$code_verification_result <- renderText(
            paste0(
              "Your activity looks suspicious to the bot detection software. ",
              "If you insist you are a human, please try again, use different browser/device,",
              "or contact support (the link below)"
            )
          )
        } else {
          session$userData$captcha_validated <- TRUE
        }
      }
      if (session$userData$captcha_validated == TRUE) {
        if (!is.null(global$beta_code)) {
          process_progress(output, input$code == global$beta_code)
        } else {
          process_progress(output, TRUE)
        }
      }
    }
  )

  observeEvent(input$button_check_code, {
    request_captcha(output, session)
  })
}
