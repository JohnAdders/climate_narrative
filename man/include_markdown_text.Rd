% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{include_markdown_text}
\alias{include_markdown_text}
\title{Converts text to HTML and passes to the output}
\usage{
include_markdown_text(text, output, output_name, add_new_tab_ref = TRUE)
}
\arguments{
\item{text}{text to be converted}

\item{output}{Shiny output}

\item{output_name}{Name of slot created in shiny output (to be used in UI part of shiny app)}

\item{add_new_tab_ref}{if TRUE (the default) all the links will be appended with target="_blank" (to open in the new window)}
}
\description{
This function should be used on the server side.
Corresponding UI side call should be: uiOutput(output_name)
All links found in the markdown are modified to be open in the new page (by adding target = "_blank")
}
