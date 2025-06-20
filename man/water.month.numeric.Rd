% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TNC_IHA_Ports.R
\name{water.month.numeric}
\alias{water.month.numeric}
\title{water.month.numeric}
\usage{
\method{water.month}{numeric}(x, label = FALSE, abbr = TRUE)
}
\arguments{
\item{x}{a date or time object or a numeric representing a calendar year month}

\item{label}{logical TRUE will return an ordered factor for month with the
month name as labels, FALSE will return a numeric}

\item{abbr}{logical. FALSE will abbreviate the name of the month in the ordered factor.}
}
\value{
Returns the month in the water year that would be that number away
}
\description{
water.month.numeric
}
