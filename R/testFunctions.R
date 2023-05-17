library(testthat)
library(cli)
source('R/autotests.R')

#' Scalar Tests
#'
#' Completes all of the basic tests for a scalar value
#' @param variableName The name of the variable in question
#' @param variables List of variables in environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param check_datatype Boolean indicating whether data type will be checked.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is double.
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testScalar <- function(variableName, variables, studentSoln, actualSoln, check_datatype=FALSE, datatype='double') {
  # Check if variable is present
  variableExistsTest(variableName, variables)

  # Run tests
  if (check_datatype) {
    dataTypeTest(studentSoln, datatype)
  }
  correctSolnTest(studentSoln, actualSoln, type="scalar")
}

#' Vector Tests
#'
#' Completes all of the tests for a list
#' @param variableName The name of the variable in question
#' @param variables List of variables in environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param check_datatype Boolean indicating whether data type will be checked.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is list
#' @param order Boolean indicating if order matters in correctness.
#' Default is TRUE.
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testVector <- function(variableName, variables, studentSoln, actualSoln, check_datatype=FALSE, datatype='list', order=TRUE) {
  # Check if variable is present
  variableExistsTest(variableName, variables)

  # Run tests
  if (check_datatype) {
    dataTypeTest(studentSoln, datatype)
  }
  correctSizeTest(studentSoln, actualSoln, type="list")
  correctSolnTest(studentSoln, actualSoln, order, type="list")
}

#' DataFrame Tests
#'
#' Completes all of the tests for a DataFrame
#' @param variableName The name of the variable in question
#' @param variables List of variables in environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param check_datatype Boolean indicating whether data type will be checked.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is list.
#' @param order Boolean indicating if order matters in correctness.
#' Default is FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testDataFrame <- function(variableName, variables, studentSoln, actualSoln, check_datatype=FALSE, datatype='list', order=FALSE) {
  # Check if variable is present
  variableExistsTest(variableName, variables)

  # Run tests
  if (check_datatype) {
    dataTypeTest(studentSoln, datatype)
  }
  correctSizeTest(studentSoln, actualSoln, type="dataframe")
  correctAttributes(studentSoln, actualSoln)
  variableClassTest(studentSoln, actualSoln)
  correctSolnTest(studentSoln, actualSoln, order, type="dataframe")
}

#' Source List
#'
#' Extracts all variables from the R file into a list
#' @param path Filepath
#' @return A list of variables from specified environment
#' @export
sourceList <- function(path) {
  e <- new.env()
  try(source(path, local = e))
  val <- as.list(e)
  return(val)
}
