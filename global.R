library(yaml)
library(rmarkdown)
library(tippy)
library(uuid)
library(shinythemes)

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
  QuestionTab$new(tab0_ui, NULL, tab0_server, 1, NULL, 1),
  QuestionTab$new(tab1_ui, NULL, tab1_server, 2, 1, 3),
  QuestionTab$new(tab2_ui, tab2_foot, NULL, 3, 2, 7),
  QuestionTab$new(tab3_ui, tab3_foot, NULL, 4, 2, 6),
  QuestionTab$new(tab4_ui, tab4_foot, NULL, 5, 2, 7),
  QuestionTab$new(tab5_ui, tab5_foot, NULL, 6, 2, 7),
  QuestionTab$new(tab6_ui, NULL, tab6_server, 7, 1, NULL)
)
