% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statsTable1.R
\name{statsTable1}
\alias{statsTable1}
\title{statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API}
\usage{
statsTable1(cty, place, sYr, eYr, ACS, oType)
}
\arguments{
\item{cty}{the County  FIPS code, including the state value}

\item{place}{the Place FIPS Code, including the state value.}

\item{sYr}{Start Year}

\item{eYr}{End year}

\item{ACS}{American Cummunity Survey Data series}

\item{oType}{Controls the rendering of the table, HTML or Latex}
}
\value{
kable formatted table
}
\description{
statsTable1 outputs the summary table in the stats section of the dashboard, draws data from the census API
}
