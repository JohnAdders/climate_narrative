library(yaml)
library(markdown)
# reading the R files (QuestionTab class, helper functions, code for tabs UI and server
sapply(dir(path='R'), function(filename) source(paste0('R/',filename)))
# reading the questions from csv files
exposures <- lapply(dir(path='exposure'), function(filename) read.csv(paste0('exposure/',filename), stringsAsFactors = FALSE))
names(exposures) <- sapply(dir(path='exposure'), function(file) strsplit(file,'.',fixed=T)[[1]][1])
# reading the user submitted yaml files
scenarios <- read_yaml_dir('scenario')
products <- read_yaml_dir('product')
exposure_classes <- read_yaml_dir('exposure_class')
# defining the questionnaire using list of QuestionTab objects
tabs <- list( 
  QuestionTab$new(tab1_ui, tab1_server, 1, NULL, 2),
  QuestionTab$new(tab2_ui, NULL, 2, 1, 6),
  QuestionTab$new(tab3_ui, NULL, 3, 1, 5),
  QuestionTab$new(tab4_ui, NULL, 4, 1, 6),
  QuestionTab$new(tab5_ui, NULL, 5, 3, 6),
  QuestionTab$new(tab6_ui, tab6_server, 6, 1, NULL)
)
