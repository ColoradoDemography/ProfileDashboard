% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demography_functions.R
\name{ed.cat}
\alias{ed.cat}
\title{Categorize a continuous education variable into common categories.}
\usage{
ed.cat(data, edvar, groups = "basic", factor = TRUE)
}
\arguments{
\item{data}{Data Frame with the education variable to categorize.}

\item{edvar}{Name of the education variable.}

\item{groups}{Categorization Scheme: more, basic, higher}

\item{factor}{Return a factor or not.  Defaults to TRUE}
}
\value{
A factor, unless otherwise specified, with recoded \code{edvar}.
}
\description{
Categorize a continuous education variable into common categories.
}
\examples{
ed.cat()
}
\keyword{education,}
\keyword{recode}
