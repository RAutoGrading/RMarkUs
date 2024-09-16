#' Test the existence of the environmental variable MARKUS_GROUP if the file is on MarkUs 
#' @param markus_environment_variable The name of the environmental variable on MarkUs
#' @return A string of the environmental variable, empty if the environmental
#' does not exist.
#' @export
is_running_on_MarkUs <- function(markus_environment_variable="MARKUS_GROUP"){
  MARKUS_GROUP <- Sys.getenv(c(markus_environment_variable))
  # get the environmental variable MARKUS_GROUP
  return(isFALSE(MARKUS_GROUP==""))
  # return true if the variable exists 
  # i.e., the variable MARKUS_GROUP is not an empty string
}