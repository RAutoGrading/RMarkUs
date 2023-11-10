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
  if (is.null(error_message)) {
    error_message <- paste("Missing", variableName)
  }
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
#' @param variableName The name of the variable in question
#' @param studentSoln The student's solution loaded from their assignment
#' @param datatype A string representing the datatype expected for the solution
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
dataTypeTest <- function(variableName, studentSoln, datatype, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect data type"
  }
  success_message = "Correct data type"
  test_name <- paste(variableName, "datatype test")
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
#' @param variableName The name of the variable in question
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param datatype A string representing the datatype expected for the solution
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
variableClassTest <- function(variableName, studentSoln, actualSoln, datatype, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect variable classes"
  }
  success_message = "Correct variable classes"
  test_name <- paste(variableName, "variable class test")
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

#' Test Correct Length (for vector, scalar, or list)
#'
#' Tests to see if the length of the student solution list is equal to the length of the actual solution list
#' @param variableName The name of the variable in question
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, vector, list
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctLengthTest <- function(variableName, studentSoln, actualSoln, type, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect length"
  }
  success_message = "Correct length"

  if (type=="vector" | type=="list") {
    test_name <- paste(variableName, "correct length test")
    tryCatch (
      {
        test_that(test_name, {expect_equal(length(studentSoln), length(actualSoln))})
        print(success_message)
      },
      error = function(e) {
        message(error_message)
      }
    )
  } else if (type == "scalar") {
    test_name <- paste(variableName, "correct length test")
    tryCatch (
      {
        test_that(test_name, {expect_equal(length(studentSoln), length(actualSoln))})
        print(success_message)
      },
      error = function(e) {
        message(error_message)
      }
    )
  }
}


#' Test Correct Size
#'
#' Tests to see if the length of the student solution list is equal to the length of the actual solution list
#' @param variableName The name of the variable in question
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, vector, dataframe
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctSizeTest <- function(variableName, studentSoln, actualSoln, type, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect variable size"
  }
  success_message = "Correct variable size"
  if (type=="vector") {
    test_name <- paste(variableName, "correct length test")
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
    test_name <- paste(variableName, "correct dimensions test")
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
#' @param variableName The name of the variable in question
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, vector, dataframe
#' @param order Whether the order of the data matters for lists. Default is TRUE.
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctSolnTest <- function(variableName, studentSoln, actualSoln, order=TRUE, type, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect answer"
  }
  success_message = "Correct answer"
  test_name <- paste(variableName, "correct value")

  if (order==FALSE & type == 'vector') {
    studentSoln <- sort(studentSoln)
    actualSoln <- sort(actualSoln)
  }
  if (order==FALSE & type == 'dataframe') {
    studentSoln <- studentSoln[order(names(studentSoln))]
    actualSoln <- actualSoln[order(names(actualSoln))]
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
#' @param variableName The name of the variable in question
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctAttributes <- function(variableName, studentSoln, actualSoln, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect attributes"
  }
  success_message = "Correct attributes"
  test_name <- paste(variableName, "correct attributes")
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
