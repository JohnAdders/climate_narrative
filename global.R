library(yaml)
library(rmarkdown)
# helper function to read all yaml/csv/R files from a directory as a named R list
read_dir <- function(directory, file_format='auto'){
  file_list <- dir(path=directory)
  file_format <- tolower(file_format)
  if (file_format == 'auto'){
    file_format <- tolower(strsplit(file_list[1], '.', fixed=T)[[1]][2])
  }
  list <- lapply(
    file_list,
    function(file) {
      switch(file_format,
        yml=read_yaml(paste0(directory, '/', file)),
        csv=read.csv(paste0(directory, '/', file), stringsAsFactors=FALSE),
        r=source(paste0(directory, '/', file)),
        stop('Error (function read_dir): file format ', file_format, ' not handled')
      )
    }
  )
  names(list) <- sapply(
    dir(path=directory),
    function(file) {
      strsplit(file, '.', fixed=T)[[1]][1]
    }
  )
  return(list)
}

read_dir('R')
exposures <- read_dir('exposure')
scenarios <- read_dir('scenario')
products <- read_dir('product')
exposure_classes <- read_dir('exposure_class')

# defining the questionnaire using list of QuestionTab objects
tabs <- list( 
  QuestionTab$new(tab1_ui, tab1_server, 1, NULL, 2),
  QuestionTab$new(tab2_ui, NULL, 2, 1, 6),
  QuestionTab$new(tab3_ui, NULL, 3, 1, 5),
  QuestionTab$new(tab4_ui, NULL, 4, 1, 6),
  QuestionTab$new(tab5_ui, NULL, 5, 3, 6),
  QuestionTab$new(tab6_ui, tab6_server, 6, 1, NULL)
)

# # defining the function that produces the ultimate description, depending on inputs
# update_final_page <- function(input, output, session) {
#   summary = list(
#     h1("Climate Narrative"),
#     p("Some Text."),
#     p("Some more Text."),
#     p("Dummy output. The exact mechanics TBC, this should produce a text basing on the inputs, for instance: "),
#     p(paste0("The type of undertaking is ",input$type,". Equity exposure to gas sector is ", input$gas_Equity,"."))
#   )
#   output$summary <- renderUI(summary)
# }
