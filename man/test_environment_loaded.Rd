% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_environment_loaded.R
\name{test_environment_loaded}
\alias{test_environment_loaded}
\title{Test to check that environment loaded properly}
\usage{
test_environment_loaded(environment, error_message = NULL)
}
\arguments{
\item{environment}{Name of the R object containing a loaded R session. Note that it should be a variable not a string!
This should be a list with one element for each variable in the loaded environment}

\item{error_message}{A function that will generate the appropriate error message as a string for if the environment is NULL
Default is NULL.}
}
\value{
Error message if one exists, otherwise will print that test has passed
}
\description{
Test to check that environment loaded properly
}
