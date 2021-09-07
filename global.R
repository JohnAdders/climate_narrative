library(yaml)

# reading the file with QuestionTab class
source('R/QuestionTab.R') 
# reading some helper functions that produce the required tables
source('R/table_functions.R') 
# reading the code for tabs UI and server
sapply(dir(path='R',pattern='tab[0-9]'), function(filename) source(paste0('R/',filename)))
# reading the questions from csv files
bank_exposures <- read.csv("csv/bank_exposures.csv", stringsAsFactors = FALSE)
insurance_assets <- read.csv("csv/insurance_assets.csv", stringsAsFactors = FALSE)
insurance_liabilities <- read.csv("csv/insurance_liabilities.csv", stringsAsFactors = FALSE)
am_exposures <- read.csv("csv/am_exposures.csv", stringsAsFactors = FALSE)
# reading the pieces from which the final report is combined
report_pieces <- read_yaml('report.yml')
# defining a shortcut to add element to the list
add_param <- function(previous_list, item_to_add) {
  c(previous_list, list(item_to_add))
}

# defining the questionnaire using list of QuestionTab objects
tabs <- list( 
  QuestionTab$new(tab1_ui, tab1_server, 1, NULL, 2),
  QuestionTab$new(tab2_ui, NULL, 2, 1, 6),
  QuestionTab$new(tab3_ui, NULL, 3, 1, 5),
  QuestionTab$new(tab4_ui, NULL, 4, 1, 6),
  QuestionTab$new(tab5_ui, NULL, 5, 3, 6),
  QuestionTab$new(tab6_ui, tab6_server, 6, 1, NULL)
)
