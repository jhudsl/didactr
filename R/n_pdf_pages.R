#' Count number of pages in PDF
#'
#' @param file input from \code{\link{check_course}}
#'
#' @return number of pages in pdf
#' @export
#' @importFrom pdftools pdf_info
#'
n_pdf_pages = function(file) {
  if (length(file) == 0 | is.na(file)) {
    return(NA)
  }
  pdftools::pdf_info(file)$pages
}


