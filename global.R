library(yaml)
library(rmarkdown)
library(tippy)
library(uuid)
library(shinythemes)
library(shinyjs)
library(httr)
library(jsonlite)

# helper function to remove special characters
remove_special_characters = function(text) gsub('[_. ]', '',text)

# helper function to read all yaml/csv/R files from a directory as a named R list
read_dir <- function(directory, file_format='auto', remove_special_characters_from_names=TRUE){
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
  names_to_be <- sapply(
    dir(path=directory),
    function(file) {
      strsplit(file, '.', fixed=T)[[1]][1]
    }
  )
  if(remove_special_characters_from_names) names_to_be <- remove_special_characters(names_to_be)
  names(list) <- names_to_be
  return(list)
}

read_dir('R')
exposures <- read_dir('exposure')
scenarios <- read_dir('scenario')
products <- read_dir('product')
exposure_classes <- read_dir('exposure_class')

# ordering the scenarios
scenarios <- scenarios[order(sapply(scenarios, `[[`, i = "position"))]

# defining the questionnaire using list of QuestionTab objects
ordered_tabs <- c('title','auth','type','bank','ins_a','am','ins_l','report')
#tab_mapping <- factor(1:length(ordered_tabs), levels=ordered_tabs)

tabs <- list(
  QuestionTab$new('title', NULL, 'auth', FALSE, FALSE),
  QuestionTab$new('auth', 'title', 'type'),
  QuestionTab$new('type', NULL, 'ins_a'),
  QuestionTab$new('bank', 'type', 'report'),
  QuestionTab$new('ins_a', 'type', 'ins_l'),
  QuestionTab$new('am', 'type','report'),
  QuestionTab$new('ins_l', 'ins_a', 'report'),
  QuestionTab$new('report', 'type', NULL)
)
