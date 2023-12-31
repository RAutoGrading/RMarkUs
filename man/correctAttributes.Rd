% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autotests.R
\name{correctAttributes}
\alias{correctAttributes}
\title{Test Correct Attributes}
\usage{
correctAttributes(
  variableName,
  variables,
  studentSoln,
  actualSoln,
  error_message = NULL
)
}
\arguments{
\item{variableName}{The name of the variable in question}

\item{variables}{List of variables in the student solution environment}

\item{studentSoln}{The student's solution loaded from their assignment}

\item{actualSoln}{The actual solution for the question}

\item{error_message}{A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.}
}
\value{
Message for a successful test or an error message if fails
}
\description{
Tests to see if the attributes of the student's DataFrame matches the attributes of the solution's DataFrame
}
