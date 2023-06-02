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
#' @param check_present Boolean indicating whether a test will check if the variable exists.
#' Default is TRUE.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is double.
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testScalar <- function(variableName, variables, studentSoln, actualSoln,
                       check_present=TRUE, check_correct=TRUE,
                       check_datatype=FALSE, datatype='double') {
  if (check_present) {
    variableExistsTest(variableName, variables)
  }

  if (check_datatype) {
    dataTypeTest(studentSoln, datatype)
  }

  if (check_correct) {
    correctSolnTest(studentSoln, actualSoln, type="scalar")
  }
}

#' Vector Tests
#'
#' Completes all of the tests for a list
#' @param variableName The name of the variable in question
#' @param variables List of variables in environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param check_present Boolean indicating whether a test will check if the variable exists.
#' Default is TRUE.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param check_size Boolean indicating whether a test will check if the size of the vector is correct.
#' Default is TRUE.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is list
#' @param order Boolean indicating if order matters in correctness.
#' Default is TRUE.
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testVector <- function(variableName, variables, studentSoln, actualSoln,
                       check_present=TRUE, check_correct=TRUE, check_size=TRUE,
                       check_datatype=FALSE, datatype='list', order=TRUE) {
  if (check_present) {
    variableExistsTest(variableName, variables)
  }

  if (check_datatype) {
    dataTypeTest(studentSoln, datatype)
  }

  if (check_size) {
    correctSizeTest(studentSoln, actualSoln, type="list")
  }

  if (check_correct) {
    correctSolnTest(studentSoln, actualSoln, order, type="list")
  }
}

#' DataFrame Tests
#'
#' Completes all of the tests for a DataFrame
#' @param variableName The name of the variable in question
#' @param variables List of variables in environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param check_present Boolean indicating whether a test will check if the variable exists.
#' Default is TRUE.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param check_size Boolean indicating whether a test will check if the size of the dataframe is correct.
#' Default is TRUE.
#' @param check_attributes Boolean indicating whether a test will check if the attributes of the dataframe are correct.
#' Default is TRUE.
#' @param check_class Boolean indicating whether a test will check if the variable classes of the dataframe are correct.
#' Default is TRUE.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is list.
#' @param order Boolean indicating if order matters in correctness.
#' Default is FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testDataFrame <- function(variableName, variables, studentSoln, actualSoln,
                          check_present=TRUE, check_correct=TRUE, check_size=TRUE,
                          check_attributes=TRUE, check_class=TRUE,
                          check_datatype=FALSE, datatype='list', order=FALSE) {
  if (check_present) {
    variableExistsTest(variableName, variables)
  }

  if (check_datatype) {
    dataTypeTest(studentSoln, datatype)
  }

  if (check_size) {
    correctSizeTest(studentSoln, actualSoln, type="dataframe")
  }

  if (check_attributes) {
    correctAttributes(studentSoln, actualSoln)
  }

  if (check_class) {
    variableClassTest(studentSoln, actualSoln)
  }

  if (check_correct) {
    correctSolnTest(studentSoln, actualSoln, order, type="dataframe")
  }

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
