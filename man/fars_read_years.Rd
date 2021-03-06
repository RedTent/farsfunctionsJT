% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions_package0.1.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Helper function to create a summary of FARS data}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{, a vector with the years of FARS you want read from file. The years will be coerced to integers}
}
\value{
function returns a list with a tibble for each year that was supplied. Each tibble in the list
contains the months and years of the observations
}
\description{
This function is a helper function for the fars_summarize_years It reads the FARS data for
the years that are provided and makes a list with a tibble for each year that was supplied.
Each tibble in the list contains the months and years of the observations
}
\details{
make sure that the files for the years that are provided in the years argument do exist.
The function will create a warning if there isn't a file for the year that is provided
}
\examples{
\dontrun{
x <- fars_read_years(c(2013,2014,2015))
}
}

