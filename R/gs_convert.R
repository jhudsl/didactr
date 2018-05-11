#' Convert Google Slides to PNGs and a script
#'
#' @param id File id passed to \code{\link{drive_download}} after passing
#' through \code{\link{as_id}}
#' @param verbose logical, indicating whether to print informative messages,
#' passed to \code{\link{drive_download}}
#' @param auto_stub Should the prefix name be taken from
#' the Google slide name?  If so, `stub` argument will
#' be overridden in \code{\link{pdf_to_images}}
#' @param ... Additional options to send to \code{\link{pdf_to_images}}
#'
#' @return A list of the images and the notes for each script
#' @export
#'
#' @importFrom googledrive drive_download as_id drive_get
gs_convert = function(id, verbose = TRUE,
                      auto_stub = TRUE, ...) {
  pdf_file = tempfile(fileext = ".pdf")

  args = list(...)
  if (auto_stub) {
    if ("stub" %in% names(args)) {
      warning(paste0(
        "Stub was passed into gs_convert, but autostub",
        "is TRUE, not using your stud"))
    }
    lesson_name = googledrive::drive_get(id = as_id(id))$name
    lesson_name = paste0(lesson_name, "-%0d")
    args$stub = lesson_name
  }
  dl = drive_download(as_id(id),
                      path = pdf_file,
                      type = "pdf")

  pptx_file = tempfile(fileext = ".pptx")
  pptx_dl = drive_download(as_id(id),
                           path = pptx_file,
                           type = "pptx")

  script = pptx_notes(pptx_file)
  args$pdf_file = pdf_file
  pngs = do.call(pdf_to_images, args)
  L = list(images = pngs,
           script = script,
           pdf_file = pdf_file,
           pptx_file = pptx_file)
  return(L)
}
