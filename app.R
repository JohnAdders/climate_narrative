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

global$exposure_classes <- read_dir("inst/exposure_class")
global$exposures <- read_dir("inst/exposure")
global$scenarios <- read_dir("inst/scenario")
global$sections <- read_dir("inst/section")
global$products <- read_dir("inst/product")
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
