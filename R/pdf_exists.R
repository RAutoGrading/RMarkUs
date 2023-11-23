#' Check for pdf
#'
#' Test to check if pdf file was submitted
#' @param Optional parameter for the name of the pdf file you're interested in checking for.
#' If none is provided, the function will just check that a pdf file exists
#' @return A list of variables from specified environment
#' @export
test_pdf_present <- function(filename=NULL) {
  if(is.null(filename)){
    pdffiles <- list.files(pattern = "*.pdf")
  }
  else{
    pdffiles <- list.files(pattern = paste(filename, ".pdf", sep=""))
  }

  test_that("PDF present:", {
    expect_true(!identical(pdffiles, character(0)))
  })
}# end test_pdf_present function
