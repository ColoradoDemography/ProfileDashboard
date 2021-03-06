% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agePlotPRO.R
\name{agePlotPRO}
\alias{agePlotPRO}
\title{agePlotPRO Creates a Chart comparing The age distribution of a selected place to the state for a simgle year}
\usage{
agePlotPRO(fips, ctyname, state = 0, yrs, base = 10, agegroup = "ten")
}
\arguments{
\item{fips}{is the numeric fips code for the main area to be compared}

\item{ctyname}{is the cplace name from input$unit}

\item{state}{is the numeric state code , it defaults to 0 in the county_sya call}

\item{yrs}{is the single year value to be extracted by county_sya}

\item{base}{is the abse text size for the ggplot2 object and codemog_theme()}
}
\value{
ggplot2 graphic and data file
}
\description{
agePlotPRO Creates a Chart comparing The age distribution of a selected place to the state for a simgle year
}
