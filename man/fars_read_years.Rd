% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Reading several FARS datasets}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{a vector containig a years from which the data will be collected.}
}
\value{
The function returns \code{data frame} containig all specified years.
   If the year would be provided wrong the function returns \code{NULL}.
}
\description{
The function is designed to automaticaly load the data from user specific years.
If the specified years would have mistake a "invalid year" warrning occurs.
}
\note{
You must include tidyr package \code{library(tidyr)}.
The data files should be named "accident_X.csv.bz2" and be in the same directory.
}
\examples{
\dontrun{fars_read_years( c(2013 ))
fars_read_years( c(2013,2014 ))
fars_read_years( c('2013','2014'))}

}
