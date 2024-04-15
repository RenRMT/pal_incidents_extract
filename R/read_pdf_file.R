#' Read UNDSS PDF Report
#'
#' @param path Path to the PDF report
#' @param collapse TRUE: returns entire PDF file as length-one character vector.
#' FALSE: Returns character vector with one element per page. Set to TRUE by default
#'
#' @return Returns a character vector of length one, or length equal to the number of pages
#' in the PDF file.
#' @export
#'
read_pdf_file <- function(path, collapse = TRUE) {
  dat <- pdftools::pdf_text(path)

  if (collapse) {
    dat <- paste(dat, collapse = "")
  }
}
