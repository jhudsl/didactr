#' Convert Google Slides to PNGs and a script
#'
#' @param id File id passed to \code{\link{drive_download}} after passing
#' through \code{\link{as_id}}
#' @param verbose logical, indicating whether to print informative messages,
#' passed to \code{\link{drive_download}}
#' @param ... Additional options to send to \code{\link{pdf_to_images}}
#'
#' @return A list of the images and the notes for each script
#' @export
#'
#' @importFrom googledrive drive_download as_id
gs_convert = function(id, verbose = TRUE, ...) {
  pdf_file = tempfile(fileext = ".pdf")

  dl = drive_download(as_id(id),
                      path = pdf_file,
                      type = "pdf")

  pptx_file = tempfile(fileext = ".pptx")
  pptx_dl = drive_download(as_id(id),
                           path = pptx_file,
                           type = "pptx")

  script = pptx_notes(pptx_file)
  pngs = pdf_to_images(pdf_file, ...)
  L = list(images = pngs,
           script = script,
           pdf_file = pdf_file,
           pptx_file = pptx_file)
  return(L)
}
