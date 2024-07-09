library(testthat)
library(cli)
library(utils)
library(knitr)
source("R/load_solutions.R")

#' Loads student and instructor submission file (locally or from MarkUs) and return a list of variables in both environments
#' @param instructor_file The solution file name (and path, relative to the project's root folder for the instructor solutions. Valid filetypes are .Rmd and .R
#' @param student_file The student submission file. Valid filetypes are .Rmd and .R
#' @param student_file_MarkUs The expected file name for student submission on MarkUs (and path, relative to the project's root folder. Valid filetypes are .Rmd and .R
#' @param markus_environment_variable The name of the environmental variable on MarkUs
#' @param print_variables Boolean value indicating whether the names of the variables loaded from the specified filepath should be printed. Defaults to false
#' @return List of 2 sub-lists: the first sub-list contains all variables in the instructor submission and
#' the second sub-list contains all variables in the student submission
#' @export
load_environments <- function(instructor_file,
                               student_file,
                               student_file_MarkUs, 
                               markus_environment_variable="MARKUS_GROUP", 
                               print_variables=FALSE){
  # Load instructor solutions
  instructor_environment <- load_solutions(file=instructor_file,
                                           print_variables=print_variables)
  # Load student solutions
  student_environment <- load_student_submission(file=student_file,
                                                 file_MarkUs = student_file_MarkUs,
                                                 markus_environment_variable=markus_environment_variable,
                                                 print_variables=print_variables
                                                 )
  all_submissions <- list(instructor_environment=instructor_environment, student_environment=student_environment)
  return(all_submissions)
}