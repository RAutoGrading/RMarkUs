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
    error_message <- paste("Missing", variableName, "in the student solution.")
  }
  success_message <- paste(variableName, "is present in the student solution.")
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
    error_message = paste("Data type in student solution:", class(studentSoln), 
                          "\ndoes NOT match the actual solution:", datatype)
  }
  success_message = paste("Correct data type", datatype, "is provided")
  test_name <- paste(variableName, "datatype test")

  correct_dataType <- all(class(studentSoln) == datatype)

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
    error_message = paste("The type of", variableName, "in the student solution:", as.character(sapply(studentSoln, class)), 
                          "\ndoes NOT match the actual solution:", as.character(sapply(actualSoln, class)))
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
    error_message = paste("Length of", variableName, "in the student solution:", length(studentSoln), 
                          "\ndoes NOT match the actual solution:", length(actualSoln))
  }
  success_message = "Correct length"
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

  success_message = "Correct variable size"

  #var_exists <- variableName %in% variables

  if (type=="vector") {
    test_name <- paste(variableName, "correct length test")
    if (is.null(error_message)) {
      error_message = paste("Length of", variableName, "in the student solution:", length(studentSoln), 
                            "\ndoes NOT match the actual solution:", length(actualSoln))
    }
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
    if (is.null(error_message)) {
      error_message = paste("Length of", variableName, "in the student solution:", dim(studentSoln), 
                            "\ndoes NOT match the actual solution:", dim(actualSoln))
  }
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
    error_message = paste("The attributes of", variableName, "in the student solution:", attributes(studentSoln), 
                          "\ndoes NOT match the actual solution:", attributes(actualSoln))
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
                            instructor_environment){
  # Initial validation of variableName, student_environment, and instructor_environment
  if(!(is.character(variableName))){
    stop("variableName should be a character object")
  }
  if(length(variableName) != 1){
    stop("variableName should have length equal to 1")
  }
  if(!is.list(student_environment)){
    stop("student_environment should be a list but it isn't")
  }
  if(!is.list(instructor_environment)){
    stop("instructor_environment should be a list but it isn't")
  }
}