library(testthat)
library(cli)
source('R/autotests.R')


#' testScalar
#'
#' Completes all of the basic tests for a single (scalar) value (that is, a vector of length 1), where the user can pass in the full environments from student and instructor solutions
#' @param variableName The name of the variable in question
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param instructor_environment A list of all variables in the environment from the solution file
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param correct_error_msg A function that will generate the appropriate error message as a string if the value is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_present Boolean indicating whether a test will check if the variable exists.
#' Default is TRUE.
#' @param present_error_msg A function that will generate the appropriate error message as a string for if variable does not exist.
#' Default is NULL and will use preset error message.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' If NULL, will check against the data type of the solution
#' Accepts values "numeric", "character", "logical", and "double" (if numeric, then tests pass if solution is either double or integer)
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param simplify_datatype Boolean indicating whether datatype tests will pass if a student passes a tibble when the solutions expect a primitive instead. Defaults to FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testScalar <- function(variableName,
                       student_environment,
                       instructor_environment,
                       check_present=TRUE, present_error_msg=NULL,
                       check_datatype=TRUE, data_error_msg=NULL,
                       check_correct=TRUE, correct_error_msg=NULL,
                       datatype = NULL,
                       simplify_datatype = FALSE) {

  correctArgsTest(variableName, student_environment, instructor_environment)

  # Extract information from inputs
  # This is cleaner and fixes issue of partial matching of variable names
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  studentSoln <- NULL
  if(variableName %in% variables) studentSoln <-student_environment[[variableName]]
  if(is.null(datatype)) datatype <- class(instructor_environment[[variableName]])

  # Validating inputs
  if (length(actualSoln) != 1){
    stop(paste("The actualSoln argument should have length 1, but it has length", length(actualSoln)))
  }

  if (isTRUE(check_datatype)){
    if(isFALSE(datatype %in% c("numeric", "integer", "complex", "logical", "character"))){
      stop(paste("Invalid data type:", datatype, "- Only numeric, integer, complex, character or logical values are allowed. Please verify the variable type in the instructor solution via class() function."))
    } 
    if(isFALSE(isa(actualSoln, datatype))){
      stop(paste("The data type of the variable in the instructor solution:", class(actualSoln), "\ndoes NOT match the data type specified in the function argument:", datatype, "\nYou may consider removing the datatype argument in the function call as the data type can be automatically defined based on the instructor solution."))
    }
  }

  #simplify_student_answer <- TRUE # This could be turned into a parameter to turn on or off
  # if(isTRUE(simplify_datatype)){
  #   # Try to convert the student's answer to a simpler datatype for comparison (e.g. tibble to single number)
  #   if(is.data.frame(studentSoln)){
  #     print("I'm here")
  #     warning(paste("You submitted a dataframe when an object with datatype", datatype, "was expected"))
  #     if(ncol(studentSoln)==1 & nrow(studentSoln)==1){
  #       print("We have 1 column")
  #       studentSoln <- studentSoln |> as.vector() |> unlist()
  #       datatype <- class(studentSoln)
  #     }
  #   }
  # }

  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    dataTypeTest(variableName, variables, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type="scalar", error_message=correct_error_msg)
  }
}# end testScalar function



#' Vector Tests
#'
#' Completes the tests for a vector, where the user can pass in the full environments from student and instructor solutions
#' @param variableName The name of the variable in question
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param instructor_environment A list of all variables in the environment from the solution file
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' Options are `numeric`, `character`, `logical`, and `double` (if numeric, then tests pass if solution is either double or integer)
#' @param type Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is vector. Options are `vector`, `scalar`, `list`
#' @param order Boolean indicating if order matters in correctness.
#' Default is TRUE.
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param correct_error_msg A function that will generate the appropriate error message as a string if the value is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_present Boolean indicating whether a test will check if the variable exists.
#' Default is TRUE.
#' @param present_error_msg A function that will generate the appropriate error message as a string for if variable does not exist.
#' Default is NULL and will use preset error message.
#' @param check_length Boolean indicating whether a test will check if the length of the vector is correct.
#' Default is TRUE.
#' @param size_error_msg A function that will generate the appropriate error message as a string if the size is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param simplify_datatype Boolean indicating whether datatype tests will pass if a student passes a tibble when the solutions expect a primitive instead. Defaults to FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testVector <- function(variableName,
                       student_environment,
                       instructor_environment,
                       datatype = NULL,
                       type='vector',
                       order=TRUE,
                       check_correct=TRUE, correct_error_msg=NULL,
                       check_present=TRUE, present_error_msg=NULL,
                       check_length=TRUE, size_error_msg=NULL,
                       check_datatype=TRUE, data_error_msg=NULL,
                       simplify_datatype=FALSE) {

  correctArgsTest(variableName, student_environment, instructor_environment)

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  studentSoln <- NULL
  if(variableName %in% variables) studentSoln <-student_environment[[variableName]]
  if(is.null(datatype)) datatype <- class(instructor_environment[[variableName]])

  # Validating inputs
  if (isTRUE(check_datatype)){
    if(isFALSE(datatype %in% c("numeric", "integer", "complex", "logical", "character"))){
      stop(paste("Invalid data type:", datatype, "- Only numeric, integer, complex, character or logical values are allowed. Please verify the variable type in the instructor solution via class() function."))
    }
    if(isFALSE(isa(actualSoln, datatype))){
      stop(paste("The data type of the variable in the instructor solution:", class(actualSoln), "\ndoes NOT match the data type specified in the function argument:", datatype, "\nYou may consider removing the datatype argument in the function call as the data type can be automatically defined based on the instructor solution."))
    }
  }


  #simplify_student_answer <- TRUE # This could be turned into a parameter to turn on or off
  # if(isTRUE(simplify_datatype)){
  #   # Try to convert the student's answer to a simpler datatype for comparison (e.g. tibble to single number)
  #   if(is.data.frame(studentSoln)){
  #     warning(paste("You submitted a dataframe when an object with datatype", datatype, "was expected"))
  #     if(ncol(studentSoln)==1 | nrow(studentSoln)==1){
  #       studentSoln <- studentSoln |> as.vector() |> unlist()
  #       datatype <- class(studentSoln)
  #     }
  #   }
  # }


  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    dataTypeTest(variableName, variables, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_length)) {
    correctLengthTest(variableName, variables, studentSoln, actualSoln, type=type, error_message=size_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type=type, order, error_message=correct_error_msg)
  }
}# end testVector


#' DataFrame Tests
#'
#' Completes all of the tests for a DataFrame, where the user can pass in the full environments from student and instructor solutions
#' @param variableName The name of the variable in question
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param instructor_environment A list of all variables in the environment from the solution file
#' @param check_correct Boolean indicating whether a test will check if the solution is correct.
#' Default is TRUE.
#' @param correct_error_msg A function that will generate the appropriate error message as a string if the value is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_present Boolean indicating whether a test will check if the variable exists.
#' Default is TRUE.
#' @param present_error_msg A function that will generate the appropriate error message as a string for if variable does not exist.
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
#' @param col_order Boolean indicating if order matters in correctness.
#' Default is FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testDataFrame <- function(variableName,
                          student_environment,
                          instructor_environment,
                          check_correct=TRUE, correct_error_msg=NULL,
                          check_present=TRUE, present_error_msg=NULL,
                          check_size=TRUE, size_error_msg=NULL,
                          check_attributes=FALSE, attributes_error_msg=NULL,
                          check_class=TRUE, class_error_msg=NULL,
                          col_order=TRUE){

  correctArgsTest(variableName, student_environment, instructor_environment)

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  studentSoln <- NULL
  if(variableName %in% variables) studentSoln <-student_environment[[variableName]]

  # Validating inputs

  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_size)) {
    correctSizeTest(variableName, variables, studentSoln, actualSoln, type="dataframe", error_message=size_error_msg)
  }

  if (isTRUE(check_attributes)) {
    correctAttributes(variableName, variables, studentSoln, actualSoln, error_message=attributes_error_msg)  ## NCM: This test fails even when I pass it two copies of the same object...
  }

  if (isTRUE(check_class)) {
    variableClassTest(variableName, variables, studentSoln, actualSoln, error_message=class_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type="dataframe", order=col_order, error_message=correct_error_msg)
  }

}# end testDataFrame function



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
}# end sourceList function



### Other ideas?
# tetsList to test more general model objects (like output of linear models, survival models, etc?)
# but maybe the above aren't feasible if minor differences in how they were run make a difference in
# the object. Instead, could test equality of several components, but not the whole object because it isn't meaningful
# This should just be one test, not several, because generally you wouldn't just get some wrong...?
# Maybe could have one test checking if it's output from the right kind of function? But keep in mind there
# may be several functions that can be used to fit a linear model for example

