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

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  if(variableName %in% variables){
    studentSoln <- student_environment[[variableName]]
  }
  else{
    studentSoln <- NULL
  }
  if(is.null(datatype)){
    datatype <- class(instructor_environment[[variableName]])
  }

  testScalar_raw(variableName = variableName,
                 variables = variables,
                 studentSoln = studentSoln,
                 actualSoln = actualSoln,
                 check_correct = check_correct, correct_error_msg=correct_error_msg,
                 check_present = check_present, present_error_msg=present_error_msg,
                 check_datatype = check_datatype, data_error_msg=data_error_msg,
                 datatype = datatype,
                 simplify_datatype=simplify_datatype)
}# end testScalar


#' testScalar_raw
#'
#' Completes all of the basic tests for a single (scalar) value (that is, a vector of length 1), where the user can directly input values to be compared
#' @param variableName The name of the variable in question
#' @param variables Vector listing the names of variables in the student's environment
#' @param studentSoln The student's answer, loaded from their submission
#' @param actualSoln The actual answer, loaded from the solution file
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
#' Accepts values "numeric", "character", "logical", and "double" (if numeric, then tests pass if solution is either double or integer)
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param simplify_datatype Boolean indicating whether datatype tests will pass if a student passes a tibble when the solutions expect a primitive instead. Defaults to FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testScalar_raw <- function(variableName,
                           variables,
                           studentSoln,
                           actualSoln,
                           check_present=TRUE, present_error_msg=NULL,
                           check_datatype=FALSE, data_error_msg=NULL,
                           check_correct=TRUE, correct_error_msg=NULL,
                           datatype=NULL,
                           simplify_datatype = FALSE){
  # Validating inputs
  if (length(actualSoln) != 1){
    stop("The testScalar function expects a value of length 1 for the actualSoln argument and but the value passed has length greater than 1")
  }

  if (isTRUE(check_datatype)){
    if (is.null(datatype)){
      stop("The testScalar function requires a value for datatype when check_datatype=TRUE.")
    }

    if(!(datatype %in% c("numeric", "character", "logical", "double"))){
      stop("The testScalar function received an invalid value for the datatype argument; valid values are numeric, character, and logical")
    }

    if (datatype == "numeric" & !is.numeric(actualSoln)){
      stop("In testScalar you indicated the datatype was 'numeric' but actualSoln is not a numeric; the datatype of actualSoln should agree with the value passed for datatype")
    }
    else if (datatype == "logical" & !is.logical(actualSoln)){
      stop("In testScalar you indicated the datatype was 'logical' but actualSoln is not a logical; the datatype of actualSoln should agree with the value passed for datatype")
    }
    else if (datatype == "character" & !is.character(actualSoln)){
      stop("In testScalar you indicated the datatype was 'character' but actualSoln is not a character object; the datatype of actualSoln should agree with the value passed for datatype")
    }
    else if (datatype == "double" & !is.double(actualSoln)){
      stop("In testScalar you indicated the datatype was 'double' but actualSoln is not a double object; the datatype of actualSoln should agree with the value passed for datatype")
    }
  }

  #simplify_student_answer <- TRUE # This could be turned into a parameter to turn on or off
  if(isTRUE(simplify_datatype)){
    # Try to convert the student's answer to a simpler datatype for comparison (e.g. tibble to single number)
    if(is.data.frame(studentSoln)){
      print("I'm here")
      warning(paste("You submitted a dataframe when an object with datatype", datatype, "was expected"))
      if(ncol(studentSoln)==1 & nrow(studentSoln)==1){
        print("We have 1 column")
        studentSoln <- studentSoln %>% as.vector() %>% unlist()
        datatype <- class(studentSoln)
      }
    }
  }

  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    if (datatype == "numeric"){
      # If student solution is integer, convert to double to facilitate comparison
      if(is.integer(studentSoln)){
        studentSoln <- as.double(studentSoln)
      }
      # In this case, we'll call dataTypeTest with 'double' because
      datatype <- "double"
    }

    dataTypeTest(variableName, variables, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type="scalar", error_message=correct_error_msg)
  }
}# end testScalar_raw function



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

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  if(variableName %in% variables){
    studentSoln <- student_environment[[variableName]]
  }
  else{
    studentSoln <- NULL
  }
  if(is.null(datatype)){
    datatype <- class(instructor_environment[[variableName]])
  }

  testVector_raw(variableName = variableName,
                 variables = variables,
                 studentSoln = studentSoln,
                 actualSoln = actualSoln,
                 check_present = check_present, present_error_msg=present_error_msg,
                 check_correct = check_correct, correct_error_msg=correct_error_msg,
                 check_datatype = check_datatype, data_error_msg=data_error_msg,
                 check_length = check_length, size_error_msg=NULL,
                 type=type,
                 order=order,
                 datatype=datatype,
                 simplify_datatype=simplify_datatype)



}# end testVector

#' Completes the tests for a vector, where the user can directly input values to be compared
#' @param variableName The name of the variable in question
#' @param variables Vector listing the names of variables in the student's environment
#' @param studentSoln The student's answer, loaded from their submission
#' @param actualSoln The actual answer, loaded from the solution file
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
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' Options are `numeric`, `character`, `logical`, and `double` (if numeric, then tests pass if solution is either double or integer)
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param type Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is vector. Options are `vector`, `scalar`, `list`
#' @param order Boolean indicating if order matters in correctness.
#' Default is TRUE.
#' @param simplify_datatype Boolean indicating whether datatype tests will pass if a student passes a tibble when the solutions expect a primitive instead. Defaults to FALSE
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testVector_raw <- function(variableName, variables, studentSoln, actualSoln,
                           check_correct=TRUE, correct_error_msg=NULL,
                           check_present=TRUE, present_error_msg=NULL,
                           check_length=TRUE, size_error_msg=NULL,
                           check_datatype=FALSE, data_error_msg=NULL,
                           datatype=NULL,
                           type='vector',
                           order=TRUE,
                           simplify_datatype=FALSE) {

  # Validating inputs
  if (isTRUE(check_datatype)){
    if (is.null(datatype)){
      stop("The testVector function requires a value for datatype when check_datatype=TRUE.")
    }

    if(!(datatype %in% c("numeric", "character", "logical", "double"))){
      stop("The testVector function received an invalid value for the datatype argument; valid values are numeric, character, and logical")
    }

    if (datatype == "numeric" & !is.numeric(actualSoln)){
      stop("In testVector you indicated the datatype was 'numeric' but actualSoln is not a numeric; the datatype of actualSoln should agree with the value passed for datatype")
    }
    else if (datatype == "logical" & !is.logical(actualSoln)){
      stop("In testVector you indicated the datatype was 'logical' but actualSoln is not a logical; the datatype of actualSoln should agree with the value passed for datatype")
    }
    else if (datatype == "character" & !is.character(actualSoln)){
      stop("In testVector you indicated the datatype was 'character' but actualSoln is not a character object; the datatype of actualSoln should agree with the value passed for datatype")
    }
    else if (datatype == "double" & !is.double(actualSoln)){
      stop("In testVector you indicated the datatype was 'double' but actualSoln is not a double object; the datatype of actualSoln should agree with the value passed for datatype")
    }
  }

  if (type == "vector" & !is.vector(actualSoln)){
    stop("In testVector, you passed type='vector' but actualSoln is not a vector")
  }
  if (type == "scalar" & length(actualSoln) > 1){
    stop("In testVector, you passed type=scalar but actualSoln has length greater than 1.")
  }


  #simplify_student_answer <- TRUE # This could be turned into a parameter to turn on or off
  if(isTRUE(simplify_datatype)){
    # Try to convert the student's answer to a simpler datatype for comparison (e.g. tibble to single number)
    if(is.data.frame(studentSoln)){
      warning(paste("You submitted a dataframe when an object with datatype", datatype, "was expected"))
      if(ncol(studentSoln)==1 | nrow(studentSoln)==1){
        studentSoln <- studentSoln %>% as.vector() %>% unlist()
        datatype <- class(studentSoln)
      }
    }
  }


  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    if (datatype == "numeric"){
      # If student solution is integer, convert to double to facilitate comparison
      if(is.integer(studentSoln)){
        studentSoln <- as.double(studentSoln)
      }
      # In this case, we'll call dataTypeTest with 'double' because
      datatype <- "double"
    }

    dataTypeTest(variableName, variables, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_length)) {
    correctLengthTest(variableName, variables, studentSoln, actualSoln, type=type, error_message=size_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type=type, order, error_message=correct_error_msg)
  }

}# end testVector_raw


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

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  if(variableName %in% variables){
    studentSoln <- student_environment[[variableName]]
  }
  else{
    studentSoln <- NULL
  }

  testDataFrame_raw(variableName = variableName,
                    variables = variables,
                    studentSoln = studentSoln,
                    actualSoln = actualSoln,
                    check_present = check_present, present_error_msg=present_error_msg,
                    check_correct = check_correct, correct_error_msg=correct_error_msg,
                    check_attributes = check_attributes, attributes_error_msg=attributes_error_msg,
                    check_size = check_size, size_error_msg=size_error_msg,
                    col_order=col_order)

}# end testDataFrame function


#' Completes all of the tests for a DataFrame, where the user can directly input values to be compared
#' @param variableName The name of the variable in question
#' @param variables Vector listing the names of variables in the student's environment
#' @param studentSoln The student's answer, loaded from their submission
#' @param actualSoln The actual answer, loaded from the solution file
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
testDataFrame_raw <- function(variableName,
                              variables,
                              studentSoln,
                              actualSoln,
                              check_correct=TRUE, correct_error_msg=NULL,
                              check_present=TRUE, present_error_msg=NULL,
                              check_size=TRUE, size_error_msg=NULL,
                              check_attributes=FALSE, attributes_error_msg=NULL,
                              check_class=TRUE, class_error_msg=NULL,
                              col_order=TRUE) {

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

}# end testDataFrame_raw function



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

