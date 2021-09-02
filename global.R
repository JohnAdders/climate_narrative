# reading the file with QuestionTab class
source('QuestionTab.R') 
# reading some helper functions that produce the required tables
source('table_functions.R') 
# reading the code for tabs UI and server
sapply(dir(pattern='tab[0-9]'), source)
# reading the questions from csv files
bank_exposures <- read.csv("bank_exposures.csv", stringsAsFactors = FALSE)
insurance_assets <- read.csv("insurance_assets.csv", stringsAsFactors = FALSE)
insurance_liabilities <- read.csv("insurance_liabilities.csv", stringsAsFactors = FALSE)
am_exposures <- read.csv("am_exposures.csv", stringsAsFactors = FALSE)

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
  QuestionTab$new(tab6_ui, tab6_server, 6, 2, NULL)
)

# defining the function that produces the ultimate description, depending on inputs
update_final_page <- function(input, output, session) {
  summary = list(
    h1("Climate Narrative"),
    h2('CURRENT STAGE: Aggregated inputs (ordered by materiality)') ,
    tableOutput("show_aggregated_inputs"),
    h2("TO DO: implement the following mechanics"),
    pre("
      for scenario in (2.5,4)
        for item in AggregatedTypeInputs
          if(scenario==2.5)
            text(item,physical,low,always)
            if(item.materiality=='H')
              text(item,physical,low,extra)
            text(item,transition,high,always)
            if(item.materiality=='H')
              text(item,transition,high,extra)
          else
            text(item,physical,high)
            if(item.materiality=='H')
              text(item,physical,low,extra)"
      ),
      p('where ',code("text(item,physical/transition,low/high,always/extra)"),
      ' are defined in some text files'),
      p('or maybe markdown files. Then it is possible to convert using markdown::markdownToHtml
      or even R snippets directly using knitr::knit')
  )
  output$summary <- renderUI(summary)
}