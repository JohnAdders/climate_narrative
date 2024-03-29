% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{filter_inputs}
\alias{filter_inputs}
\title{Filter the inputs depending on institution type, sector. Optionally aggregate and override all materialities}
\usage{
filter_inputs(all_inputs_table, filter_settings)
}
\arguments{
\item{all_inputs_table}{data frame of all inputs (usually the reactive expression all_inputs)}

\item{filter_settings}{list of relevant setting, comprising of:
\itemize{
\item inst_type institution type to filter (or "" for no filtering)
\item sector sector to filter (or "" for no filtering)
\item aggregate bool, whether to aggregate the inputs by sector
\item override_materiality ignore the actual inputs and set all materialities to a level (no override by default)
}}
}
\description{
Filter the inputs depending on institution type, sector. Optionally aggregate and override all materialities
}
