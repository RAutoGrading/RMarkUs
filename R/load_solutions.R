#' Loads solutions file and return a list of variables in the environment
#' @param file The file name (and path, relative to the project's root folder. Valid filetypes are .Rmd and .R
#' @param print_variables Boolean value indicating whether the names of the variables loaded from the specified filepath should be printed. Defaults to false
#' @return List of all variables in the instructor solutions
#' @export
load_solutions <- function(file, print_variables=FALSE){
  if(isTRUE(grepl(pattern=".rmd", x=file, ignore.case=TRUE))){
    vars <- sourceList(knitr::purl(file, quiet=TRUE))
  } else if(isTRUE(grepl(pattern=".r", x=file, ignore.case=TRUE))){
    # What to do if .R file is passed?
  }
  else{
    stop("The file passed to load_solutions does not have a supported extension; only .Rmd and .R files are accepted")
  }
  if(isTRUE(print_variables)){
    print(names(vars))
  }
  return(vars)
}# end load_solutions

#' Loads student submission file (locally or from MarkUs) and return a list of variables in the environment
#' @param file The local file name (and path, relative to the project's root folder for the instructor solutions. Valid filetypes are .Rmd and .R
#' @param file_MarkUs The expected file name for student submissions on MarkUs (and path, relative to the project's root folder. Valid filetypes are .Rmd and .R
#' @param filepath_local_root String used to identify whether the current environment is on the user's local computer or on MarkUs' servers.
#' Defaults to "C:/" but can be changed if the user isn't working in the C drive. To specify a good value for this, users can use `getwd()` to identify the root of their local filepath
#' @param print_variables Boolean value indicating whether the names of the variables loaded from the specified filepath should be printed. Defaults to false
#' @return List of all variables in the student submission
#' @export
load_student_submission <- function(file,
                                    file_MarkUs = NULL,
                                    filepath_local_root = "C:/",
                                    print_variables=FALSE){

  if(isTRUE(grepl(pattern=filepath_local_root, x=getwd(), ignore.case = TRUE))){
    # In this case we'll load the local version of the student submission
    student_vars <- load_solutions(file = file, print_variables=print_variables)
  } else{
    if(is.null(file_MarkUs)){
      stop("Missing filename for student submissions on MarkUs")
    }
    # In this case, we're on MarkUs
    student_vars <- load_solutions(file = file_MarkUs, print_variables=print_variables)
  }
  if(is.null(file_MarkUs)){
    warning("Missing filename for student submissions on MarkUs - make sure to specify it before uploading tests to MarkUs")
  }

  return(student_vars)
}
