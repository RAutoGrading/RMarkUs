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
#' @param present_error_msg A function that will generate the appropriate error message as a string for if variable does not exist.
#' Default is NULL and will use preset error message.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param correct_error_msg A function that will generate the appropriate error message as a string if the value is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is double.
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' #' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testScalar <- function(variableName, variables, studentSoln, actualSoln,
                       check_present=TRUE, present_error_msg=NULL,
                       check_correct=TRUE, correct_error_msg=NULL,
                       check_datatype=FALSE, datatype='double', data_error_msg=NULL) {
  if (check_present) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (check_datatype) {
    dataTypeTest(studentSoln, datatype, error_message=data_error_msg)
  }

  if (check_correct) {
    correctSolnTest(studentSoln, actualSoln, type="scalar", error_message=correct_error_msg)
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
#' @param present_error_msg A function that will generate the appropriate error message as a string for if variable does not exist.
#' Default is NULL and will use preset error message.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param correct_error_msg A function that will generate the appropriate error message as a string if the value is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_size Boolean indicating whether a test will check if the size of the vector is correct.
#' Default is TRUE.
#' @param size_error_msg A function that will generate the appropriate error message as a string if the size is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is list
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param order Boolean indicating if order matters in correctness.
#' Default is TRUE.
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testVector <- function(variableName, variables, studentSoln, actualSoln,
                       check_present=TRUE, present_error_msg=NULL,
                       check_correct=TRUE, correct_error_msg=NULL,
                       check_size=TRUE, size_error_msg=NULL,
                       check_datatype=FALSE, datatype='list', order=TRUE, data_error_msg=NULL) {
  if (check_present) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (check_datatype) {
    dataTypeTest(studentSoln, datatype, error_message=data_error_msg)
  }

  if (check_size) {
    correctSizeTest(studentSoln, actualSoln, type="list", error_message=size_error_msg)
  }

  if (check_correct) {
    correctSolnTest(studentSoln, actualSoln, type="list", order, error_message=correct_error_msg)
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
#' @param present_error_msg A function that will generate the appropriate error message as a string for if variable does not exist.
#' Default is NULL and will use preset error message.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param correct_error_msg A function that will generate the appropriate error message as a string if the value is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_size Boolean indicating whether a test will check if the size of the dataframe is correct.
#' Default is TRUE.
#' @param size_error_msg A function that will generate the appropriate error message as a string if the size is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_attributes Boolean indicating whether a test will check if the attributes of the dataframe are correct.
#' Default is TRUE.
#' @param attributes_error_msg A function that will generate the appropriate error message as a string if the attributes of the dataframe are incorrect.
#' Default is NULL and will use preset error message.
#' @param check_class Boolean indicating whether a test will check if the variable classes of the dataframe are correct.
#' Default is TRUE.
#' @param class_error_msg A function that will generate the appropriate error message as a string if the classes of the dataframe are incorrect.
#' Default is NULL and will use preset error message.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is list.
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param order Boolean indicating if order matters in correctness.
#' Default is FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testDataFrame <- function(variableName, variables, studentSoln, actualSoln,
                          check_present=TRUE, present_error_msg=NULL,
                          check_correct=TRUE, correct_error_msg=NULL,
                          check_size=TRUE, size_error_msg=NULL,
                          check_attributes=TRUE, attributes_error_msg=NULL,
                          check_class=TRUE, class_error_msg=NULL,
                          check_datatype=FALSE, datatype='list', order=FALSE, data_error_msg=NULL) {
  if (check_present) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (check_datatype) {
    dataTypeTest(studentSoln, datatype, error_message=data_error_msg)
  }

  if (check_size) {
    correctSizeTest(studentSoln, actualSoln, type="dataframe", error_message=size_error_msg)
  }

  if (check_attributes) {
    correctAttributes(studentSoln, actualSoln, error_message=attributes_error_msg)
  }

  if (check_class) {
    variableClassTest(studentSoln, actualSoln, error_message=class_error_msg)
  }

  if (check_correct) {
    correctSolnTest(studentSoln, actualSoln, type="dataframe", order, error_message=correct_error_msg)
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
