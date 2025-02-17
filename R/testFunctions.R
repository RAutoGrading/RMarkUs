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
                       student_environment=student_environment,
                       instructor_environment=instructor_environment,
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
  if(is.null(datatype)) datatype <- class(actualSoln)

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
                       student_environment=student_environment,
                       instructor_environment=instructor_environment,
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
  if(is.null(datatype)) datatype <- class(actualSoln)

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
                          student_environment=student_environment,
                          instructor_environment=instructor_environment,
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
    # Reorder student solution based on instructor solution if we do not check order
    if (isFALSE(col_order) & isFALSE(is.null(studentSoln))) {
      student_sol_reorder <- names(actualSoln)
      studentSoln <- studentSoln[student_sol_reorder]
    }
    variableClassTest(variableName, variables, studentSoln, actualSoln, error_message=class_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type="dataframe", order=col_order, error_message=correct_error_msg)
  }

}# end testDataFrame function

#' Compare if 2 linear models are the same
#'
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
#' @param round_precision The number of decimal places RSS and df will be rounded. Default is 3.
#' @param AIC_compare Optional parameter that enables AIC comparison of models. Default is FALSE.
#'
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
#'
testLinearModel <- function(variableName,
                            student_environment=student_environment,
                            instructor_environment=instructor_environment,
                            check_correct=TRUE, correct_error_msg=NULL,
                            check_present=TRUE, present_error_msg=NULL,
                            AIC_compare = FALSE, round_precision=3){

  correctArgsTest(variableName, student_environment, instructor_environment)

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  studentSoln <- NULL
  if(variableName %in% variables) studentSoln <-student_environment[[variableName]]
  datatype <- class(instructor_environment[[variableName]])[1]
  if(isFALSE(datatype == 'lm')){
    stop(paste("Invalid data type:", datatype, "- Only lm object is allowed. Please verify the model class in the instructor solution via class() function."))
  }
  # if(isFALSE(class(studentSoln) == 'lm')){
  #   stop(paste("Invalid data type:", class(studentSoln), "- Only lm object is allowed. Please verify the model class in the student solution via class() function."))
  # }
  anova_table <- stats::anova(actualSoln, studentSoln)
  if (isTRUE(AIC_compare)){
    actualSoln <- round(stats::AIC(actualSoln),round_precision)
    studentSoln <- round(stats::AIC(studentSoln),round_precision)
  }
  actualSoln <- c(actualSoln, round(as.numeric(anova_table[1,c("Res.Df","RSS")]),round_precision))
  studentSoln <- c(studentSoln, round(as.numeric(anova_table[2,c("Res.Df","RSS")]),round_precision))

  # Validating inputs

  # Unit tests
  if (isTRUE(check_present)) {
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_correct)) {
    correctSolnTest(variableName, variables, studentSoln, actualSoln, type='vector', order=F, error_message=correct_error_msg)
  }

}# end testDataFrame function

#' Compare if a particular column has the correct group and whether the number of
#' observation in each group is correct.
#'
#' @param variableName The name of the variable in question
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param instructor_environment A list of all variables in the environment from the solution file
#' @param subset Boolean indicating whether another column needs to be selected.
#' Use if an entire dataframe is passed to variableName.
#' Default is False
#' @param column_name A string of the column name
#'
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
#'
testGroupNumber <- function(variableName,
                            student_environment=student_environment,
                            instructor_environment=instructor_environment,
                            subset = FALSE, column_name = NULL
                            ){
  correctArgsTest(variableName, student_environment=student_environment, instructor_environment=instructor_environment)

  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  studentSoln <- NULL
  if(variableName %in% variables) studentSoln <- student_environment[[variableName]]

  if(isTRUE(subset)) {
    if(isTRUE(is.null(column_name))) {
      stop("Column name should be provided if subset is TRUE.")
    }
    actualSoln <- table(actualSoln[[column_name]])
    if(isFALSE(is.null(studentSoln))) {studentSoln <- table(studentSoln[[column_name]])}
  }
  summary_table_name = paste(variableName,"summary_table_RMarkUs",sep='_')
  instructor_environment[[summary_table_name]] = actualSoln
  student_environment[[summary_table_name]] = studentSoln
  testDataFrame(variableName = summary_table_name,student_environment = student_environment, instructor_environment = instructor_environment, col_order=FALSE)
}

#' Check if the ggplot object is correct.
#'
#' @param variableName The name of the variable in question
#' @param student_environment A list of all variables in the environment from the student's submission
#' @param expected_x_axis_var: A vector of possible variables for the x-axis of the plot; test will pass if any one of these variables is on the x-axis of the student's plot
#'  Defaults to NULL (in which case no test run)
#' @param expected_y_axis_var: A vector of possible variables for the y-axis of the plot; test will pass if any one of these variables is on the x-axis of the student's plot
#'  Defaults to NULL (in which case no test run)
#' @param instructor_environment A list of all variables in the environment from the solution file. Default is `NULL`
#' @param plot_type A string indicating the plot type. User can choose from `histogram` or `boxplot`. Default is `boxplot`
#' @param check_x Boolean. Whether to check if x is correct. Defaults to TRUE.
#' @param x_error_msg Character or NULL. Custom error message if x variable fails the check. Defaults to NULL.
#' @param check_y Boolean. Whether to check if y is correct. Defaults to TRUE.
#' @param y_error_msg Character or NULL. Custom error message if y variable fails the check. Defaults to NULL.
#' @param check_present Boolean. Whether to check if required variables are present. Defaults to TRUE.
#' @param present_error_msg Character or NULL. Custom error message if the variable is missing. Defaults to NULL.
#' @param check_x_label Boolean. Whether to check if x has a descriptive label. Defaults to TRUE.
#' @param x_label_error_msg Character or NULL. Custom error message if x label check fails. Defaults to NULL.
#' @param check_y_label Boolean. Whether to check if y has a descriptive label. Defaults to TRUE.
#' @param y_label_error_msg Character or NULL. Custom error message if y label check fails. Defaults to NULL.
#' @param check_class Boolean. Whether to check if x and y have the expected class. Defaults to `TRUE`.
#' @param class_error_msg Character or NULL. Custom error message if class validation fails. Defaults to NULL.
#'
#' @return Error message if one exists, otherwise will print that every test has passed.
#' @export
#'
testPlot <- function(variableName,
                    student_environment=student_environment,
                    expected_x_axis_var = NULL,
                    expected_y_axis_var = NULL,
                    instructor_environment=NULL,
                    plot_type="boxplot",
                    check_x=TRUE, x_error_msg=NULL,
                    check_y=TRUE, y_error_msg=NULL,
                    check_present=TRUE, present_error_msg=NULL,
                    check_x_label=TRUE, x_label_error_msg=NULL,
                    check_y_label=TRUE, y_label_error_msg=NULL,
                    check_class=TRUE, class_error_msg=NULL
){
  if(isFALSE(is.null(instructor_environment))){
  correctArgsTest(variableName, student_environment=student_environment, instructor_environment=instructor_environment)
  }
  # Extract information from inputs
  actualSoln <- instructor_environment[[variableName]]
  variables <- names(student_environment) # this should be a list of the names
  studentSoln <- NULL
  if(variableName %in% variables) studentSoln <- student_environment[[variableName]]

  if (isTRUE(check_present)) {
    present_error_msg <- paste(variableName, "is not present in the student solution.")
    variableExistsTest(variableName, variables, error_message=present_error_msg)
  }

  if (isTRUE(check_class)) {
    if (isTRUE(plot_type %in% "boxplot")){
    expected_geom <- "GeomBoxplot"
    class_error_msg <- paste('It should be a boxplot (GeomBoxplot), not a',class(student_environment[[variableName]]$layers[[1]]$geom)[1])
    }
    if (isTRUE(plot_type %in% "histogram")){
      expected_geom <- "GeomCol"
      class_error_msg <- paste('It should be a histogram (GeomCol), not a',class(student_environment[[variableName]]$layers[[1]]$geom)[1])
    }
    test_that(class_error_msg, {
      expect_true(expected_geom %in% class(student_environment[[variableName]]$layers[[1]]$geom))
    })
    print("Correct plot type.")
  }

  if (isTRUE(check_x)) {
    student_x_label <- as.character(na.omit(c(
      rlang::get_expr(student_environment[[variableName]]$mapping$x),
      rlang::get_expr(student_environment[[variableName]]$layers[[1]]$mapping$x))))
    instructor_x_label <- NULL
    if (isFALSE(is.null(instructor_environment))){
      instructor_x_label <- as.character(na.omit(c(
      rlang::get_expr(instructor_environment[[variableName]]$mapping$x),
      rlang::get_expr(instructor_environment[[variableName]]$layers[[1]]$mapping$x)))
    )}
    x_error_msg <- paste(student_x_label, 'should be on the x-axis.')
    test_that(x_error_msg, {
      expect_true(student_x_label %in% c(instructor_x_label,expected_x_axis_var))
    })
    print("Correct x axis.")
  }

  if (isTRUE(check_y)) {
    student_y_label <- as.character(na.omit(c(
      rlang::get_expr(student_environment[[variableName]]$mapping$y),
      rlang::get_expr(student_environment[[variableName]]$layers[[1]]$mapping$y))))
    instructor_y_label <- NULL
    if (isFALSE(is.null(instructor_environment))){
      instructor_y_label <- as.character(na.omit(c(
        rlang::get_expr(instructor_environment[[variableName]]$mapping$y),
        rlang::get_expr(instructor_environment[[variableName]]$layers[[1]]$mapping$y)))
      )}
    y_error_msg <- paste(student_y_label, 'should be on the y-axis.')
    test_that(y_error_msg, {
      expect_true(student_y_label %in% c(instructor_y_label,expected_y_axis_var))
    })
    print("Correct y axis.")
  }

  if (isTRUE(check_x_label)) {
    x_label_student <- as.character(na.omit(c(
      rlang::get_expr(student_environment[[variableName]]$mapping$x),
      rlang::get_expr(student_environment[[variableName]]$layers[[1]]$mapping$x)
    ))[1])
    x_label_error_msg <- 'Label on the x-axis should be descriptive and human readable.'
    test_that(x_label_error_msg, {
      expect_false(student_environment[[variableName]]$labels$x == x_label_student)
    })
    print("Reasonable x label.")
  }

  if (isTRUE(check_y_label)) {
    y_label_student <- as.character(na.omit(c(
      rlang::get_expr(student_environment[[variableName]]$mapping$y),
      rlang::get_expr(student_environment[[variableName]]$layers[[1]]$mapping$y)
    ))[1])
    y_label_error_msg <- 'Label on the y-axis should be descriptive and human readable.'
    test_that(y_label_error_msg, {
      expect_false(student_environment[[variableName]]$labels$y == y_label_student)
    })
    print("Reasonable y label.")
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
}# end sourceList function



### Other ideas?
# tetsList to test more general model objects (like output of linear models, survival models, etc?)
# but maybe the above aren't feasible if minor differences in how they were run make a difference in
# the object. Instead, could test equality of several components, but not the whole object because it isn't meaningful
# This should just be one test, not several, because generally you wouldn't just get some wrong...?
# Maybe could have one test checking if it's output from the right kind of function? But keep in mind there
# may be several functions that can be used to fit a linear model for example

