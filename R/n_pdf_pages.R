#' Count number of pages in PDF
#'
#' @param file input from \code{\link{check_course}}
#'
#' @return number of pages in pdf
#' @export
#'
n_pdf_pages = function(file) {
  if (length(file) == 0 | is.na(file)) {
    return(NA)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    message(
      paste0("Package \"pdftools\" needed for n_pdf_pages to work. ",
             "Please install it."))
    return(NA)
  }
  pdftools::pdf_info(file)$pages
}


