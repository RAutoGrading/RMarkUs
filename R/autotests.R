library(testthat)
library(cli)

# Purpose: Autograding testing functions for assignment solutions


# -------------------------------------------------------------------------
# Internal Helper Function: Run Autograder Test
# -------------------------------------------------------------------------
# This helper function reduces repeated code.
#
# Original pattern repeated in almost every function:
#   tryCatch(
#     {
#       test_that(test_name, { ... expectation ... })
#       print(success_message)
#     },
#     error = function(e) {
#       message(error_message)
#     }
#   )
#
# Now, each test function only needs to define:
#   1. test_name
#   2. the check to run
#   3. success_message
#   4. error_message
#
# Note:
# The check is passed in as a function so it is evaluated inside test_that().
# -------------------------------------------------------------------------
run_autograder_test <- function(test_name, check, success_message, error_message) {
  tryCatch(
    {
      test_that(test_name, {
        check()
      })
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}


# -------------------------------------------------------------------------
# Internal Helper Function: Safe Equality Check
# -------------------------------------------------------------------------
# This helper checks whether two objects are equal without directly using:
#   expect_equal(studentSoln, actualSoln)
#
# Why?
# expect_equal(studentSoln, actualSoln) can show the expected solution when it fails.
#
# Instead:
#   all.equal(studentSoln, actualSoln)
# returns TRUE when objects are equal, and a text explanation when they are not.
#   isTRUE(...)
# converts that result into a simple TRUE/FALSE.
#
# Then we use:
#   expect_true(...)
#
# This makes the failure message less likely to reveal the instructor's answer.
# -------------------------------------------------------------------------
safe_equal <- function(studentSoln, actualSoln) {
  isTRUE(all.equal(studentSoln, actualSoln))
}


# -------------------------------------------------------------------------
# Internal Helper Function: Correct Size / Length Test
# -------------------------------------------------------------------------
# This combines the repeated logic from correctLengthTest() and correctSizeTest().
#
# It handles:
#   - scalar
#   - vector
#   - list
#   - dataframe
#
# correctLengthTest() is kept below as a wrapper so old code that calls it
# will still work.
# -------------------------------------------------------------------------
correctLengthOrSizeTest <- function(variableName,
                                    variables,
                                    studentSoln,
                                    actualSoln,
                                    type,
                                    error_message = NULL,
                                    success_message = "Correct variable size") {
  # variables is kept in the function signature for compatibility with older code.
  # It is not used directly in this function.
  # var_exists <- variableName %in% variables

  if (type %in% c("scalar", "vector", "list")) {
    test_name <- paste(variableName, "correct length test")

    if (is.null(error_message)) {
      # Do not reveal the expected length.
      error_message <- paste("Incorrect length for", variableName, "in the student solution.")
    }

    same_length <- length(studentSoln) == length(actualSoln)

    run_autograder_test(
      test_name = test_name,
      check = function() {
        expect_true(same_length)
      },
      success_message = success_message,
      error_message = error_message
    )

  } else if (type %in% c("dataframe", "data.frame")) {
    test_name <- paste(variableName, "correct dimensions test")

    if (is.null(error_message)) {
      # Do not reveal the expected dimensions.
      error_message <- paste("Incorrect dimensions for", variableName, "in the student solution.")
    }

    same_dimensions <- identical(dim(studentSoln), dim(actualSoln))

    run_autograder_test(
      test_name = test_name,
      check = function() {
        expect_true(same_dimensions)
      },
      success_message = success_message,
      error_message = error_message
    )

  } else {
    stop("Unsupported type. Use one of: scalar, vector, list, dataframe.")
  }
}


#' Test if Variable Exists
#'
#' Tests to see if the variable for a question exists in the R environment
#' @param variableName The name of the variable in question
#' @param variables List of variables in the student solution environment
#' @param error_message A function that will generate the appropriate error message as a string. Default is NULL and will use preset error message.
#' @return Message for a successful test or an error message if fails
#' @export
variableExistsTest <- function(variableName, variables, error_message = NULL) {
  if (is.null(error_message)) {
    error_message <- paste("Missing", variableName, "in the student solution.")
  }

  success_message <- paste(variableName, "is present in the student solution.")
  test_name <- paste(variableName, "Exists")

  var_exists <- variableName %in% variables

  run_autograder_test(
    test_name = test_name,
    check = function() {
      # Use expect_true() because var_exists is already TRUE/FALSE.
      expect_true(var_exists)
    },
    success_message = success_message,
    error_message = error_message
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
dataTypeTest <- function(variableName, variables, studentSoln, datatype, error_message = NULL) {
  if (is.null(error_message)) {
    # Do not reveal the expected datatype.
    error_message <- paste("Incorrect data type for", variableName, "in the student solution.")
  }

  success_message <- paste("Correct data type", datatype, "is provided")
  test_name <- paste(variableName, "datatype test")

  # variables is kept in the function signature for compatibility with older code.
  # var_exists <- variableName %in% variables

  correct_dataType <- identical(class(studentSoln), datatype) ||
    all(class(studentSoln) == datatype)

  run_autograder_test(
    test_name = test_name,
    check = function() {
      # Use expect_true() because correct_dataType is already TRUE/FALSE.
      expect_true(correct_dataType)
    },
    success_message = success_message,
    error_message = error_message
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
variableClassTest <- function(variableName, variables, studentSoln, actualSoln, error_message = NULL) {
  if (is.null(error_message)) {
    # Do not reveal the expected column classes.
    error_message <- paste("Incorrect variable classes for", variableName, "in the student solution.")
  }

  success_message <- "Correct variable classes"
  test_name <- paste(variableName, "variable class test")

  # variables is kept in the function signature for compatibility with older code.
  # var_exists <- variableName %in% variables

  # Use lapply instead of sapply because columns can sometimes have multiple classes.
  all_columns_same_classes <- identical(
    lapply(studentSoln, class),
    lapply(actualSoln, class)
  )

  run_autograder_test(
    test_name = test_name,
    check = function() {
      # Use expect_true() to avoid printing the expected class structure.
      expect_true(all_columns_same_classes)
    },
    success_message = success_message,
    error_message = error_message
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
correctLengthTest <- function(variableName, variables, studentSoln, actualSoln, type, error_message = NULL) {
  # This function is now a wrapper around correctLengthOrSizeTest().
  # It is kept so older tests that call correctLengthTest() will not break.

  correctLengthOrSizeTest(
    variableName = variableName,
    variables = variables,
    studentSoln = studentSoln,
    actualSoln = actualSoln,
    type = type,
    error_message = error_message,
    success_message = "Correct length"
  )
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
correctSizeTest <- function(variableName, variables, studentSoln, actualSoln, type, error_message = NULL) {
  # This function now uses the shared helper also used by correctLengthTest().
  # For scalar/vector/list, it checks length.
  # For dataframe, it checks dimensions.

  correctLengthOrSizeTest(
    variableName = variableName,
    variables = variables,
    studentSoln = studentSoln,
    actualSoln = actualSoln,
    type = type,
    error_message = error_message,
    success_message = "Correct variable size"
  )
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
#' @param actualOutput Boolean of whether to show desired output or not. Default is FALSE.description
#' @return Message for a successful test or an error message if fails
#' @export
correctSolnTest <- function(variableName, variables, studentSoln, actualSoln, order = TRUE, type, error_message = NULL) {
  if (is.null(error_message)) {
    # Keep this general so the expected solution is not revealed.
    error_message <- "Incorrect answer"
  }

  success_message <- "Correct answer"
  test_name <- paste(variableName, "correct value")

  # If order does not matter for vectors, sort both before comparing.
  if (order == FALSE && type == "vector") {
    studentSoln <- sort(studentSoln)
    actualSoln <- sort(actualSoln)
  }

  # If order does not matter for dataframes, reorder columns alphabetically.
  # Note: this only ignores column order, not row order.
  if (order == FALSE && type %in% c("dataframe", "data.frame")) {
    studentSoln <- studentSoln[order(names(studentSoln))]
    actualSoln <- actualSoln[order(names(actualSoln))]
  }

  # variables is kept in the function signature for compatibility with older code.
  # var_exists <- variableName %in% variables
  # all_values_equal <- all(studentSoln == actualSoln)

  solution_is_correct <- safe_equal(studentSoln, actualSoln)

  run_autograder_test(
    test_name = test_name,
    check = function() {
      # Do not use expect_equal(studentSoln, actualSoln) here.
      # That can reveal the expected solution.
      expect_true(solution_is_correct)
    },
    success_message = success_message,
    error_message = error_message
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
correctAttributes <- function(variableName, variables, studentSoln, actualSoln, error_message = NULL) {
  # Note that this function can give somewhat weird results: Consider the following example:
  # d <- tibble(v1=1:2, v2=3:4)
  # d1 <- d; d1[1,1] <- 44
  # identical(attributes(d), attributes(d1)) # The order of items in the attributes list is different even though contents are the same - modifying the object somehow changes this

  if (is.null(error_message)) {
    # Do not reveal the expected attributes.
    error_message <- paste("Incorrect attributes for", variableName, "in the student solution.")
  }

  success_message <- "Correct attributes"
  test_name <- paste(variableName, "correct attributes")

  # variables is kept in the function signature for compatibility with older code.
  # var_exists <- variableName %in% variables

  all_attributes_equal <- identical(
    attributes(studentSoln),
    attributes(actualSoln)
  )

  run_autograder_test(
    test_name = test_name,
    check = function() {
      # Use expect_true() instead of expect_identical(attributes(...), attributes(...))
      # to avoid printing expected attributes.
      expect_true(all_attributes_equal)
    },
    success_message = success_message,
    error_message = error_message
  )
}


#' Test if the arguments have correct data types
#'
#' @param variableName The name of the variable in question
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param instructor_environment A list of all variables in the environment from the solution file
#'
#' @return an error message will be provided if none of the tests passed
#' @export
correctArgsTest <- function(variableName,
                            student_environment,
                            instructor_environment) {
  # Initial validation of variableName, student_environment, and instructor_environment

  if (!(is.character(variableName))) {
    stop("variableName should be a character object")
  }

  if (length(variableName) != 1) {
    stop("variableName should have length equal to 1")
  }

  if (!is.list(student_environment)) {
    stop("student_environment should be a list but it isn't")
  }

  if (!is.list(instructor_environment)) {
    stop("instructor_environment should be a list but it isn't")
  }
}