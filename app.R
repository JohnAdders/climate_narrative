library(shiny)
library(R6)
library(httr)
library(jsonlite)
library(rmarkdown)
library(shinyjs)
library(shinythemes)
library(stringi)
library(tippy)
library(uuid)
library(yaml)
library(png)
library(markdown)
library(testthat)

read_dir_2 <- function(directory, file_format = "auto", in_package = FALSE, remove_special_characters_from_names = TRUE) {
  if (in_package) directory <- system.file(directory, package = "climate.narrative")
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

global$exposure_classes <- read_dir_2("inst/exposure_class")
global$exposures <- read_dir_2("inst/exposure")
global$scenarios <- read_dir_2("inst/scenario")
global$sections <- read_dir_2("inst/section")
global$products <- read_dir_2("inst/product")
global$content_files <- list(
    tabs = global$tabs,
    scenarios = global$scenarios,
    sections = global$sections,
    exposure_classes = global$exposure_classes
  )
initialise_globals()
load_secrets("secret.yml")
global$report_version <- 6
addResourcePath(
    "climate_narrative",
    "inst/www"
  )
  addResourcePath(
    "lib",
    "inst/www/lib"
  )
  options(stringsAsFactors = FALSE)
shinyApp(ui = ui(), server = server)
