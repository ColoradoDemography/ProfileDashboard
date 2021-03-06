% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ms_jobs.R
\name{ms_jobs}
\alias{ms_jobs}
\title{ms_jobs Creates a Chart showing the Total Estimates Jobs series for each County in Colorado}
\usage{
ms_jobs(fips, ctyname, maxyr, base = 10)
}
\arguments{
\item{fips}{is the fips code for the county being examined}

\item{ctyname}{This parameter puts the name of the county in the chart}

\item{maxyr}{The maximum year value, from CurYr}

\item{base}{is the abse text size for the ggplot2 object and codemog_theme()}
}
\value{
ggplot graphic and data file
}
\description{
Modified from ms_jobs January, 2018 AB
}
\details{
Uses State Demography Office data to create a chart showing the timeseries of Total Estimated Jobs
(which means it includes Proprietors and Agricultural Workers) for a selected Colorado County
}
