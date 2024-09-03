library(testthat)
library(cli)
library(utils)
library(knitr)

#' Wrapper for testing student number, essentially testScalar on student number
#'
#' @param varname default is the student number
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param instructor_environment A list of all variables in the environment from the solution file
#'
#' @return It will print that every test has passed.
#' @export
#'

test_student_number <- function (varname="STUDENT_NUMBER",
                                 student_environment=student_environment,
                                 instructor_environment=instructor_environment){
  testScalar(variableName = varname,
             student_environment = student_environment,
             instructor_environment = instructor_environment,
             datatype="numeric")
}
