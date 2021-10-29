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
ordered_tabs <- c('auth','type','bank','ins_a','am','ins_l','report')
#tab_mapping <- factor(1:length(ordered_tabs), levels=ordered_tabs)

tabs <- list(
  QuestionTab$new('auth', NULL, 'type'),
  QuestionTab$new('type', NULL, 'ins_a'),
  QuestionTab$new('bank', 'type', 'report'),
  QuestionTab$new('ins_a', 'type', 'ins_l'),
  QuestionTab$new('am', 'type','report'),
  QuestionTab$new('ins_l', 'type', 'report'),
  QuestionTab$new('report', 'type', NULL)
)
