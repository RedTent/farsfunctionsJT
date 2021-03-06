% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions_package0.1.R
\name{make_filename}
\alias{make_filename}
\title{Helper function to create a characterstring with the name of a FARS datafile of specific year}
\usage{
make_filename(year)
}
\arguments{
\item{year}{, the year of which you want to load the FARS data. Should be an integer. The function will try to coerce this to an integer}
}
\value{
returns a character string of the filename to be loaded, can be used together with the fars_read() function
}
\description{
Helper function to create a characterstring with the name of a FARS datafile of specific year
}
\examples{
\dontrun{
make_filename(year=2015)
make_filename(2015)
}
}

