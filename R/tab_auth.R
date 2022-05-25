passes_captcha <- function(input, session) {
  result <- recaptcha_server(global$captcha_secret, input$responseReceived)
  if (result$success && result$score > global$captcha_threshold) {
    return(TRUE)
  } else {
    print(
      paste0(
        "Failed captcha attempt. Details: success ",
        result$success,
        "score ",
        result$score,
        "hostname ",
        result$hostname
      )
    )
    return(FALSE)
  }
}

request_captcha <- function(output, session) {
  if (!is.null(global$captcha_code) && !is.null(global$captcha_secret)) {
    recaptcha_js(global$captcha_code, "homepage", "responseReceived")
  } else {
    output$code_send_result <- renderText("Captcha configuration missing, can't proceed")
  }
}

process_progress <- function(output, should_continue) {
  if (should_continue) {
    next_tab <- tab_name_to_number("intro")
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
        tippy::tippy_this("button_check_code", "Click to proceed (if the code is correct)"),
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
        tippy::tippy_this("button_check_code", "Click to proceed"),
        textOutput("code_verification_result")
      )
    })
  }
}


tab_auth_ui <- function(captcha_code) {
  list(
    div(
      class = "disclaimer", id = "disclaimer_1",
      p("This tool is currently under development.
        During 2022, the Climate Financial Risk Forum Scenario Analysis Working Group will
        continue to collect feedback from users on this beta version of the tool up to Q3 2022
        and will update the tool to enhance content and reflect the latest NGFS scenarios in Q1 2023.
        It represents output from the cross-industry
        Scenario Analysis Working Group of the Prudential Regulation Authority and Financial
        Conduct Authority's Climate Financial Risk Forum (CFRF)."),
      p("The PRA and FCA have convened and facilitated CFRF discussions but do not accept
        liability for the views expressed in this guide which do not necessarily represent
        the view of the regulators and in any case do not constitute regulatory guidance."),
      p("The tool references data from the NGFS scenarios which were updated in June 2021.
        However, the tool has not been endorsed by the NGFS. Users who wish to learn more
        about the NGFS scenarios are directed to the NGFS scenario portal."),
      p("The information contained in this tool has been written by industry, for industry.
        The recommendations in this guide do not constitute financial or other professional
        advice and should not be relied upon as such."),
      p("The information contained in any reports can be reproduced or re-disseminated
        in any form as long as this disclaimer is not removed, the source is identified
        as the Climate Financial Risk Forum's Climate Narrative Tool and any changes from
        the original text are clearly marked."),
      p("The content in this tool is provided \"as is\" without warranty of any kind, express or implied,
        including but not limited to the warranties or merchantability, fitness for a particular
        purpose and non-infringement. In no event shall the authors or copyright holders be
        liable for any claim, damages or other liability, whether in an action of contract,
        tort or otherwise arising from, out of or in connection with the report."),
      p(strong("Copyright 2022 The Climate Financial Risk Forum"))
    ),
    hr(),
    recaptcha_ui(captcha_code),
    fluidRow(
      uiOutput("first_column")
    )
  )
}

tab_auth_server <- function(input, output, session) {
  render_dynamic_auth_ui(output, session)

  observeEvent(
    input$responseReceived,
    {
      if (session$userData$captcha_validated == FALSE) {
        captcha_result <- passes_captcha(input, session)
        if (captcha_result == FALSE) {
          warning("Captcha verification failed")
          output$code_verification_result <- renderText("Captcha verification failed")
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
