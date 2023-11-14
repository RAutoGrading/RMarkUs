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
#' Accepts values "double", "character", and "logical"
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' #' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testScalar <- function(variableName, variables, studentSoln, actualSoln,
                       check_present=TRUE, present_error_msg=NULL,
                       check_correct=TRUE, correct_error_msg=NULL,
                       check_datatype=FALSE, datatype=NULL, data_error_msg=NULL) {

  # Validating inputs
  if (length(actualSoln) != 1){
    stop("The testScalar function expects a value of length 1 for the actualSoln argument and but the value passed has length greater than 1")
  }

  if (isTRUE(check_datatype) & is.null(datatype)){
    stop("The testScalar function requires a value for datatype when check_datatype=TRUE.")
  }

  if(isTRUE(check_datatype) & !(datatype %in% c("double", "character", "logical"))){
    stop("The testScalar function received an invalid value for the datatype argument; valid values are double, character, and logical")
  }

  if (datatype == "double" & !is.double(actualSoln)){
    stop("In testScalar you indicated the datatype was 'double' but actualSoln is not a double; the datatype of actualSoln should agree with the value passed for datatype")
  }
  else if (datatype == "logical" & !is.logical(actualSoln)){
    stop("In testScalar you indicated the datatype was 'logical' but actualSoln is not a logical; the datatype of actualSoln should agree with the value passed for datatype")
  }
  else if (datatype == "character" & !is.character(actualSoln)){
    stop("In testScalar you indicated the datatype was 'character' but actualSoln is not a character object; the datatype of actualSoln should agree with the value passed for datatype")
  }


  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    dataTypeTest(variableName, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, studentSoln, actualSoln, type="scalar", error_message=correct_error_msg)
  }
}



#' Vector Tests
#'
#' Completes all of the tests for a vector
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
#' @param check_length Boolean indicating whether a test will check if the length of the vector is correct.
#' Default is TRUE.
#' @param size_error_msg A function that will generate the appropriate error message as a string if the size is incorrect.
#' Default is NULL and will use preset error message.
#' @param check_datatype Boolean indicating whether a test will check if the data type is correct.
#' Default is FALSE.
#' @param datatype Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' Options are `double`, `character`, `logical`
#' @param type Optional argument (string) for data type expected for solution if check_datatype is TRUE.
#' DEFAULT is vector. Options are `vector`, `scalar`, `list`
#' @param data_error_msg A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @param order Boolean indicating if order matters in correctness.
#' Default is TRUE.
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
testVector <- function(variableName, variables, studentSoln, actualSoln,
                       check_present=TRUE, present_error_msg=NULL,
                       check_correct=TRUE, correct_error_msg=NULL,
                       check_length=TRUE, size_error_msg=NULL,
                       check_datatype=FALSE,
                       datatype=NULL,
                       type='vector', order=TRUE, data_error_msg=NULL) {


  # Validating inputs
  if (isTRUE(check_datatype) & is.null(datatype)){
    stop("The testVector function requires a value for datatype when check_datatype=TRUE.")
  }

  if(isTRUE(check_datatype) & !(datatype %in% c("double", "character", "logical"))){
    stop("The testVector function received an invalid value for the datatype argument; valid values are double, character, and logical")
  }

  if (type == "vector" & !is.vector(actualSoln)){
    stop("In testVector, you passed type='vector' but actualSoln is not a vector")
  }
  if (type == "scalar" & length(actualSoln) > 1){
    stop("In testVector, you passed type=scalar but actualSoln has length greater than 1.")
  }

  if (datatype == "double" & !is.double(actualSoln)){
    stop("In testVector you indicated the datatype was 'double' but actualSoln is not a double; the datatype of actualSoln should agree with the value passed for datatype")
  }
  else if (datatype == "logical" & !is.logical(actualSoln)){
    stop("In testVector you indicated the datatype was 'logical' but actualSoln is not a logical; the datatype of actualSoln should agree with the value passed for datatype")
  }
  else if (datatype == "character" & !is.character(actualSoln)){
    stop("In testVector you indicated the datatype was 'character' but actualSoln is not a character object; the datatype of actualSoln should agree with the value passed for datatype")
  }


  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    dataTypeTest(variableName, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_length)) {
    correctLengthTest(variableName, studentSoln, actualSoln, type=type, error_message=size_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, studentSoln, actualSoln, type=type, order, error_message=correct_error_msg)
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

  # Validating inputs

  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_datatype)) {
    dataTypeTest(variableName, studentSoln, datatype, error_message=data_error_msg)
  }

  if (isTRUE(check_size)) {
    correctSizeTest(variableName, studentSoln, actualSoln, type="dataframe", error_message=size_error_msg)
  }

  if (isTRUE(check_attributes)) {
    correctAttributes(variableName, studentSoln, actualSoln, error_message=attributes_error_msg)
  }

  if (isTRUE(check_class)) {
    variableClassTest(variableName, studentSoln, actualSoln, error_message=class_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, studentSoln, actualSoln, type="dataframe", order, error_message=correct_error_msg)
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
