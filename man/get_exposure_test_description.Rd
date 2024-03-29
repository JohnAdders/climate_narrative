% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_exposure_test_description}
\alias{get_exposure_test_description}
\title{Karnan's request for easier change comparison - single sector}
\usage{
get_exposure_test_description(exposure_classes, item_name, subitem_names = c())
}
\arguments{
\item{exposure_classes}{List of exposure classes (sectors)}

\item{item_name}{sector name (group level)}

\item{subitem_names}{names of the sectors (lower level), by default empty vector corresponding to no subitems}
}
\value{
vector of string - executive summary text (3 items + 1 per scenario + 1 item at the end)
}
\description{
Karnan's request for easier change comparison - single sector
}
