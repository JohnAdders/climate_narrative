library(R6)
library(yaml)
library(rmarkdown)
library(tippy)
library(uuid)
library(shinythemes)
library(shinyjs)
library(httr)
library(jsonlite)
library(stringi)

# helper function to remove special characters
remove_special_characters <- function(text, camelcase=TRUE) {
  out <- text
  if(camelcase){
    out <- gsub('\\ (\\w?)', '\\U\\1', tolower(out), perl=TRUE)
    out <- gsub('\\_(\\w?)', '\\U\\1', out, perl=TRUE)
  }
  gsub("[_. ]", "", out)
}

# helper function to read all yaml/csv/R files from a directory as a named R list
read_dir <- function(directory, file_format = "auto", remove_special_characters_from_names = TRUE) {
  file_list <- dir(path = directory)
  file_format <- tolower(file_format)
  if (file_format == "auto") {
    file_format <- tolower(strsplit(file_list[1], ".", fixed = T)[[1]][2])
  }
  list <- lapply(
    file_list,
    function(file) {
      switch(file_format,
        yml = read_yaml(paste0(directory, "/", file)),
        csv = read.csv(paste0(directory, "/", file), stringsAsFactors = FALSE),
        r = source(paste0(directory, "/", file)),
        stop("Error (function read_dir): file format ", file_format, " not handled")
      )
    }
  )
  names_to_be <- sapply(
    dir(path = directory),
    function(file) {
      strsplit(file, ".", fixed = T)[[1]][1]
    }
  )
  if (remove_special_characters_from_names) names_to_be <- remove_special_characters(names_to_be)
  names(list) <- names_to_be
  return(list)
}

read_dir("R")
exposures <- read_dir("exposure")
scenarios <- read_dir("scenario")
products <- read_dir("product")
exposure_classes <- read_dir("exposure_class")

# ordering the scenarios
scenarios <- scenarios[order(sapply(scenarios, `[[`, i = "position"))]

# defining the questionnaire using list of QuestionTab objects
ordered_tabs <- c(
  "title", "auth", "type",
  "bank_re", "bank_c", "bank_sov",
  "ins_l", "ins_nl", "ins_c", "ins_sov",
  "am_c", "am_sov", "am_re",
  "report"
)

userData <- list()
if (file.exists("secret.yml")) {
  secret_settings <- read_yaml("secret.yml")
  userData$dev <- FALSE
  for (i in 1:length(secret_settings)) userData[[names(secret_settings)[i]]] <- secret_settings[[i]]
} else {
  userData$dev <- TRUE
}

tabs <- list(
  QuestionTab$new("title", NULL, "auth", FALSE, FALSE),
  QuestionTab$new("auth", "title", NULL, ui_settings = list(captcha_code=userData$captcha_code)),
  QuestionTab$new("type", "auth", "ins_l"),
  QuestionTab$new("bank_re", "type", "bank_c", TRUE, TRUE, exposures$bankRe, "bank", "R"),
  QuestionTab$new("bank_c", "bank_re", "bank_sov", TRUE, TRUE, exposures$bankCorporate, "bank", "C"),
  QuestionTab$new("bank_sov", "bank_c", "report", TRUE, TRUE, exposures$sovereign, "bank", "S"),
  QuestionTab$new("ins_l", "type", "ins_nl", TRUE, TRUE, exposures$insuranceLife, "insurance", "L"),
  QuestionTab$new("ins_nl", "ins_l", "ins_c", TRUE, TRUE, exposures$insuranceNonlife, "insurance", "N"),
  QuestionTab$new("ins_c", "ins_nl", "ins_sov", TRUE, TRUE, exposures$insuranceCorporate, "insurance", "C"),
  QuestionTab$new("ins_sov", "ins_c", "report", TRUE, TRUE, exposures$sovereign, "insurance", "S"),
  QuestionTab$new("am_c", "type", "am_sov", TRUE, TRUE, exposures$amCorporate, "asset", "C"),
  QuestionTab$new("am_sov", "am_c", "am_re", TRUE, TRUE, exposures$sovereign, "asset", "S"),
  QuestionTab$new("am_re", "am_sov", "report", TRUE, TRUE, exposures$amRe, "asset", "R"),
  QuestionTab$new("report", "type", NULL)
)
