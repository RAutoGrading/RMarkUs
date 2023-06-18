library(testthat)
library(cli)

# Purpose: Autograding testing functions for assignment solutions

#' Test if Variable Exists
#'
#' Tests to see if the variable for a question exists in the R environment
#' @param variableName The name of the variable in question
#' @param variables List of variables in environment
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
variableExistsTest <- function(variableName, variables, error_message=NULL) {
  error_message <- paste("Missing", variableName)
  success_message <- paste(variableName, "present")
  test_name <- paste(variableName, "Exists")
  var_exists <- variableName %in% variables
  tryCatch (
    {
      test_that(test_name, {expect_equal(var_exists, TRUE)})
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}

#' Test Data Type
#'
#' Tests to see if the student's solution is of the correct data type
#' @param studentSoln The student's solution loaded from their assignment
#' @param datatype A string representing the datatype expected for the solution
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
dataTypeTest <- function(studentSoln, datatype, error_message=NULL) {
  error_message = "Incorrect data type"
  success_message = "Correct data type"
  var_name <- deparse(substitute(studentSoln))
  test_name <- paste(var_name, "datatype test")
  tryCatch (
    {
      test_that(test_name, {expect_type(studentSoln, datatype)})
      print(success_message)
    },
  error = function(e) {
    message(error_message)
  }
  )
}

#' Test Variable Class
#'
#'Tests to see if the classes of the DataFrame columns are the same
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param datatype A string representing the datatype expected for the solution
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
variableClassTest <- function(studentSoln, actualSoln, datatype, error_message=NULL) {
  error_message = "Incorrect variable classes"
  success_message = "Correct variable classes"
  var_name <- deparse(substitute(actualSoln))
  test_name <- paste(var_name, "variable class test")
  tryCatch (
    {
      test_that(test_name, {expect_identical(sapply(studentSoln, class), sapply(actualSoln, class))})
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}


#' Test Correct Size
#'
#' Tests to see if the length of the student solution list is equal to the length of the actual solution list
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, list, dataframe
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctSizeTest <- function(studentSoln, actualSoln, type, error_message=NULL) {
  error_message = "Incorrect variable size"
  success_message = "Correct variable size"
  var_name <- deparse(substitute(actualSoln))
  if (type=="list") {
    test_name <- paste(var_name, "correct length test")
    tryCatch (
      {
        test_that(test_name, {expect_equal(length(studentSoln), length(actualSoln))})
        print(success_message)
      },
      error = function(e) {
        message(error_message)
      }
    )
  } else if (type == "dataframe") {
    test_name <- paste(var_name, "correct dimensions test")
    tryCatch (
      {
        test_that(test_name, {expect_identical(dim(studentSoln), dim(actualSoln))})

        print(success_message)
      },
      error = function(e) {
        message(error_message)
      }
    )
  }
}

#' Test Correct Solution
#'
#' Tests to see if the student solution is equal to the actual solution
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, list, dataframe
#' @param order Whether the order of the data matters for lists. Default is TRUE.
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctSolnTest <- function(studentSoln, actualSoln, order=TRUE, type, error_message=NULL) {
  error_message = "Incorrect answer"
  success_message = "Correct answer"
  var_name <- deparse(substitute(actualSoln))
  test_name <- paste(var_name, "correct value")

  if (order==FALSE & type == 'list') {
    studentSoln <- sort(studentSoln)
    actualSoln <- sort(actualSoln)
  }
  tryCatch (
    {
      if (type=="dataframe") {
        test_that(test_name, {expect_identical(studentSoln, actualSoln)})
      } else {
      test_that(test_name, {expect_equal(studentSoln, actualSoln)})}
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}

#' Test Correct Attributes
#'
#' Tests to see if the attributes of the student's DataFrame matches the attributes of the solution's DataFrame
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctAttributes <- function(studentSoln, actualSoln, error_message=NULL) {
  error_message = "Incorrect attributes"
  success_message = "Correct attributes"
  var_name <- deparse(substitute(actualSoln))
  test_name <- paste(var_name, "correct attributes")
  tryCatch (
    {
      test_that(test_name, {expect_identical(attributes(studentSoln), attributes(actualSoln))})
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}
