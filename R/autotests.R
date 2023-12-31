library(testthat)
library(cli)

# Purpose: Autograding testing functions for assignment solutions

#' Test if Variable Exists
#'
#' Tests to see if the variable for a question exists in the R environment
#' @param variableName The name of the variable in question
#' @param variables List of variables in the student solution environment
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
#' @param variables List of variables in the student solution environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param datatype A string representing the datatype expected for the solution
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
dataTypeTest <- function(variableName, variables, studentSoln, datatype, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect data type"
  }
  success_message = "Correct data type"
  test_name <- paste(variableName, "datatype test")

  correct_dataType <- all(typeof(studentSoln) == datatype)

  #var_exists <- variableName %in% variables

  tryCatch (
    {
      test_that(test_name, expect_equal(correct_dataType, TRUE))
      #test_that(test_name, { expect_equal(correct_dataType, TRUE) & expect_equal(var_exists,TRUE) })
      print(success_message)
    },
  error = function(e) {
    message(error_message)
  }
  )
}

#' Test Variable Class
#'
#'Tests to see if the columns of two given dataframes have the same classes
#' @param variableName The name of the variable in question
#' @param variables List of variables in the student solution environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
variableClassTest <- function(variableName, variables, studentSoln, actualSoln, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect variable classes"
  }
  success_message = "Correct variable classes"
  test_name <- paste(variableName, "variable class test")

  #all_columns_same_classes <- all( sapply(studentSoln, class) == sapply(actualSoln, class) )

  #var_exists <- variableName %in% variables

  tryCatch (
    {
      test_that(test_name, expect_identical(sapply(studentSoln, class), sapply(actualSoln, class)))
      #test_that(test_name, {expect_identical(all_columns_same_classes, TRUE) & expect_equal(var_exists,TRUE)})
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
#' @param variables List of variables in the student solution environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, vector, list
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctLengthTest <- function(variableName, variables, studentSoln, actualSoln, type, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect length"
  }
  success_message = "Correct length"

  #var_exists <- variableName %in% variables

  if (type=="vector" | type=="list") {
    test_name <- paste(variableName, "correct length test")
    tryCatch (
      {
        test_that(test_name, expect_equal(length(studentSoln), length(actualSoln)))
        #test_that(test_name, {expect_equal(length(studentSoln), length(actualSoln)) & expect_equal(var_exists,TRUE)})
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
        test_that(test_name, expect_equal(length(studentSoln), length(actualSoln)))
        #test_that(test_name, {expect_equal(length(studentSoln), length(actualSoln)) & expect_equal(var_exists,TRUE)})
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
#' @param variables List of variables in the student solution environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, vector, dataframe
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctSizeTest <- function(variableName, variables, studentSoln, actualSoln, type, error_message=NULL) {
  if (is.null(error_message)) {
    error_message = "Incorrect variable size"
  }
  success_message = "Correct variable size"

  #var_exists <- variableName %in% variables

  if (type=="vector") {
    test_name <- paste(variableName, "correct length test")
    tryCatch (
      {
        test_that(test_name, expect_equal(length(studentSoln), length(actualSoln)))
        #test_that(test_name, {expect_equal(length(studentSoln), length(actualSoln)) & expect_equal(var_exists,TRUE)})
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
        test_that(test_name, expect_equal(dim(studentSoln), dim(actualSoln)))
        #test_that(test_name, {expect_identical(dim(studentSoln), dim(actualSoln)) & expect_equal(var_exists,TRUE)})

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
#' @param variables List of variables in the student solution environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param type The type of data. Options are: scalar, vector, dataframe
#' @param order Whether the order of the data matters for lists. Default is TRUE.
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctSolnTest <- function(variableName, variables, studentSoln, actualSoln, order=TRUE, type, error_message=NULL) {
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

  #var_exists <- variableName %in% variables
  #all_values_equal <- all(studentSoln == actualSoln)

  tryCatch (
    {
      if (type=="dataframe") {
        test_that(test_name, { expect_equal(studentSoln, actualSoln) }) # Not sure why was expect_identical before?
        #test_that(test_name, { expect_identical(studentSoln, actualSoln) })
        #test_that(test_name, { expect_identical(all_values_equal, TRUE) & expect_equal(var_exists,TRUE) })
      } else {
        test_that(test_name, { expect_equal(studentSoln, actualSoln)})
        #test_that(test_name, { expect_equal(all_values_equal, TRUE) & expect_equal(var_exists,TRUE) })
        }
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
#' @param variables List of variables in the student solution environment
#' @param studentSoln The student's solution loaded from their assignment
#' @param actualSoln The actual solution for the question
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
correctAttributes <- function(variableName, variables, studentSoln, actualSoln, error_message=NULL) {
  # Note that this function can give somewhat weird results: Consider the following example:
  # d <- tibble(v1=1:2, v2=3:4)
  # d1 <- d; d1[1,1] <- 44
  # identical(attributes(d), attributes(d1)) # The order of items in the attributes list is different even though contents are the same - modifying the object somehow changes this

  if (is.null(error_message)) {
    error_message = "Incorrect attributes"
  }
  success_message = "Correct attributes"
  test_name <- paste(variableName, "correct attributes")

  #all_attributes_equal <- identical(attributes(studentSoln), attributes(actualSoln))
  #var_exists <- variableName %in% variables

  #print(all_attributes_equal)
  #print(var_exists)

  tryCatch (
    {
      test_that(test_name, expect_identical(attributes(studentSoln), attributes(actualSoln)))
      #test_that(test_name, {expect_identical(all_attributes_equal, TRUE) & expect_equal(var_exists,TRUE)})
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}
