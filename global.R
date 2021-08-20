# reading the file with QuestionTab class
source('QuestionTab.R') 
# reading some helper functions that produce the required tables
source('table_functions.R') 
# reading the questions from csv files
bank_exposures <- read.csv("bank_exposures.csv", stringsAsFactors = FALSE)
insurance_assets <- read.csv("insurance_assets.csv", stringsAsFactors = FALSE)
insurance_liabilities <- read.csv("insurance_liabilities.csv", stringsAsFactors = FALSE)
am_exposures <- read.csv("am_exposures.csv", stringsAsFactors = FALSE)

# defining a shortcut to add element to the list
add_param <- function(previous_list, iten_to_add) {
  c(previous_list, list(iten_to_add))
}

# defining the questionnaire using list of QuestionTab objects
tabs <- list( 
  QuestionTab$new(tab1_ui, tab1_server, 1, NULL, 2),
  QuestionTab$new(tab2_ui, NULL, 2, 1, 6),
  QuestionTab$new(tab3_ui, NULL, 3, 1, 5),
  QuestionTab$new(tab4_ui, NULL, 4, 1, 6),
  QuestionTab$new(tab5_ui, NULL, 5, 3, 6),
  QuestionTab$new(tab6_ui, tab6_server, 6, 2, NULL)
)

# reading the code for tabs UI and server
sapply(tabs, function(tab) source(paste0('tab',tab$tab_number,'.R')))

# defining the function that produces the ultimate description, depending on inputs
update_final_page <- function(input, output, session) {
  summary = list(
    h1("Climate Narrative"),
    p("Some Text."),
    p("Some more Text."),
    p("Dummy output. The exact mechanics TBC, this should produce a text basing on the inputs, for instance: "),
    p(paste0("The type of undertaking is ",input$type,". Equity exposure to gas sector is ", input$gas_Equity,"."))
  )
  output$summary <- renderUI(summary)
}