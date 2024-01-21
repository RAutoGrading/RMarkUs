#' Check for pdf
#'
#' Test to check if pdf file was submitted
#' @param filename Optional parameter for the name of the pdf file you're interested in checking for.
#' If none is provided, the function will just check that a pdf file exists
#' @param error_message A function that will generate the appropriate error message as a string for if the data type is not correct.
#' Default is NULL and will use preset error message.
#' @return A list of variables from specified environment
#' @export
test_pdf_exists <- function(filename=NULL,error_message=NULL) {
  if(is.null(filename)){
    filename <- "*.pdf" # In this case we'll just check for any pdf file
  }
  else if(!any(grepl(pattern=".pdf", x = filename))){
    # In this case, the filename doesn't have a .pdf extension so we'll add it here
    filename <- paste(filename, ".pdf", sep="")
  }

  pdffiles_in_folder <- list.files(pattern = "*.pdf")
  test_name <- paste(filename, " file exists", sep="")

  if (is.null(error_message)) {
    error_message <- paste("Missing", filename, "file", sep=" ")
  }
  success_message <- paste(filename, "file is present")

  # Check if filename is present
  file_present <- any(grepl(pattern=filename, x = pdffiles_in_folder))

  tryCatch (
    {
      test_that(test_name, {expect_true(file_present)})
      print(success_message)
    },
    error = function(e) {
      message(error_message)
    }
  )
}

