ui <- function() {
    fluidPage(
        #useShinyjs(),
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "climate_narrative.css")
        ),
        # authentication - inspired by https://towardsdatascience.com/r-shiny-authentication-incl-demo-app-a599b86c54f7
        div(
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
            p('Enter the verification code received in your email, currently: hardcoded "pass"'),
            textInput(
                inputId='show_app',
                label=tagList(icon("unlock-alt"), "Verification code") , 
                placeholder = 'Enter your verification code'
            ),
            actionButton(
                inputId='show_app_button',
                label='Validate'
            ),
            hr()
        ),
        uiOutput(outputId = "display_content_basic")
    )
}