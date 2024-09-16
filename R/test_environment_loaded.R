library(testthat)
library(cli)
library(utils)
library(knitr)

#' Test to check that environment loaded properly
#' @param environment Name of the R object containing a loaded R session. Note that it should be a variable not a string!
#' This should be a list with one element for each variable in the loaded environment
#' @param error_message A function that will generate the appropriate error message as a string for if the environment is NULL
#' Default is NULL.
#' @return Error message if one exists, otherwise will print that test has passed
#' @export
#'
# test_environment_loaded <- function(environment, error_message=NULL){
#
#   test_name <- paste(deparse(substitute(environment)), "loaded")
#
#   if (is.null(error_message)) {
#     error_message <- paste(environment,
#                            "is not loaded properly and is equal to NULL. Please reach out to your course instructor because this test should always pass.")
#   }
#   success_message <- paste(deparse(substitute(environment)), "is loaded properly and has length", length(environment))
#
#   tryCatch (
#     {
#       test_that(test_name, {expect_equal(!is.null(environment), TRUE)})
#       print(success_message)
#     },
#     error = function(e) {
#       message(error_message)
#     }
#   )# end tryCatch
#   }# end test_environment_loaded function
test_environment_loaded <- function(environment, error_message=NULL){
  if(is.null(error_message)){
    error_message = "The evironment is not loaded properly. Please check if
    the file name in load_solutions function matches the actual .Rmd file name."
  }
  if(length(environment)==0){
    print(error_message)
  }
  else {
    print("The environment is loaded successfully.")
  }
}
