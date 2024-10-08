% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_student_number.R
\name{get_student_number}
\alias{get_student_number}
\title{#' set_student_number handles the student number}
\usage{
get_student_number(
  student_number = 1,
  filename_student_info = "student_list.csv",
  MarkUs_environment_variable = "MARKUS_GROUP"
)
}
\arguments{
\item{student_number}{Student number provided by students}

\item{filename_student_info}{The .csv file contains student information with 6 columns ("username", "LastName", "FirstName", "Tutorial", "StudentNumber", "email")}

\item{MarkUs_environment_variable}{The name of the environmental variable on MarkUs}
}
\value{
The student number based on who and where the tests are run. If a student runs locally, the user-defined student number will be used. When tests are run on MarkUs, then the student number will be extracted from the environmental variable.
}
\description{
#' set_student_number handles the student number
}
