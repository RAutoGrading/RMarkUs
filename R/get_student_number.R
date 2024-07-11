library(testthat)
library(cli)
library(utils)
library(knitr)
source("R/is_running_on_MarkUs.R")

# Sketch of new functionality for (1) set_student_number function and (2) test_student_number test function 

#' set_student_number handles the student number in different scenarios: it will return the true student number if it is run on MarkUs by instructors
#'
#' @param student_number Student number provided by students
#' @param solution_filename_keyword The file name of solution should contain 'sol' so that it can be used to identify whether the instructor is running the script
#' @param filename_student_info The .csv file contains student information with 6 columns ("username", "LastName", "FirstName", "Tutorial", "StudentNumber", "email")
#' @param MarkUs_environment_variable The name of the environmental variable on MarkUs
#'
#' @return The student number based on who and where the tests are run. If a student runs locally, the user-defined student number will be used. When tests are run on MarkUs, then the student number will be extracted from the environmental variable. 
#' @export
#'

get_student_number <- function(student_number = 1,
                               solution_filename_keyword = "sol",
                               filename_student_info = "student_info.csv",
                               MarkUs_environment_variable = "MARKUS_GROUP"){ 
# Call is_running_on_MarkUs(), false if not on MarkUs   
  if(isFALSE(is_running_on_MarkUs(MarkUs_environment_variable))){ 
    return(student_number) 
  } 
  else{ 
    # We know we’re running on MarkUs 
    # Extract filename of current script, using basename(rstudioapi::getSourceEditorContext()$path) and saving string in filename_current_script 
    filename_current_script <- basename(rstudioapi::getSourceEditorContext()$path)
    if(isTRUE(grepl(solution_filename_keyword, filename_current_script))){
      student_mapping <- read.csv(filename_student_info)
      names(student_mapping) <- c("username", "LastName", "FirstName", 
                                  "Tutorial", "StudentNumber", "email")
      # Get current student
      MARKUS_GROUP <- Sys.getenv(c("MARKUS_GROUP"))
      student_number <- student_mapping[student_mapping$username == MARKUS_GROUP,]$StudentNumber
    }
  } 
  return(student_number) 
} 



# Implementation of the above in the problemset.Rmd file 

# STUDENT_NUMBER <- get_student_number(student_number = 1) 

# function to check if STUDENT_NUMBER is equal in instructor and student files. Just a wrapper of testScalar I think, but the custom function name and error message would make it easier to use. 

# test_student_number(varname=STUDENT_NUMBER, 
#                     environments  # list of two lists 
# )  

# If the test above fails, it means that the student typed the wrong student number in their submission. The test should return a message like "student_number is equal to X, but the expected value is Y" 



I’m envisioning functions like test_student_number() and test_environment_loaded() (and others?) to be part of a "debugging" preamble at the top of every .R test script, right after the instructor and student environments are loaded. 