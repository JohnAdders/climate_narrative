library(yaml)
library(markdown)
# helper function to read all yaml/csv/R files from a directory as a named R list
read_dir <- function(directory, file_format='auto'){
  file_list <- dir(path=directory)
  file_format <- tolower(file_format)
  if (file_format == 'auto'){
    file_format <- strsplit(file_list[1], '.', fixed=T)[[1]][2]
  }
  list <- lapply(
    file_list,
    function(file) {
      if (file_format=='yml') {
        read_yaml(paste0(directory, '/', file))
      } else if (file_format=='csv') {
        read.csv(paste0(directory, '/', file), stringsAsFactors=FALSE)
      } else if (file_format=='r'){
        source(paste0(directory, '/', file))
      }
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
