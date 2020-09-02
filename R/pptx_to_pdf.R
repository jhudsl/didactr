#' Convert PPTX to Google Slides (and then PDF)
#'
#' @param path Path of PPTX file name
#' @param verbose print diagnostic messages
#' @param name name of PPTX, if different than base name of path.
#'
#' @return A Google Slide ID or a path to a PDF file
#' @export
#' @examples
#' path = system.file(
#' "extdata", "example.pptx", package = "didactr")
#' pdf = pptx_to_pdf(path)
pptx_to_gs = function(
  path,
  name = NULL,
  verbose = TRUE) {
  pdf_file = tempfile(fileext = ".pdf")
  if (verbose) {
    message("Converting PPTX to PDF")
  }

  didactr::check_didactr_auth()
  if (is.null(name)) {
    name = basename(tempfile())
  }
  id = googledrive::drive_upload(
    media = path,
    verbose = TRUE,
    type = googledrive::drive_mime_type("presentation"),
    name = name,
    overwrite = TRUE)

  return(id)
}

#' @export
#' @rdname pptx_to_gs
#' @param trash should the Google Slide be trashed after downloading
#' the PDF?
pptx_to_pdf = function(
  path,
  name = NULL,
  trash = TRUE,
  verbose = TRUE) {

  id = pptx_to_gs(
    path = path,
    name = name,
    verbose = verbose)

  if (trash) {
    on.exit({
      googledrive::drive_trash(id)
    })
  }

  download_wrapper = function(id, path, type, get_result) {
    ttype = toupper(type)
    dl = try({
      googledrive::drive_download(
        id,
        path = path,
        type = type)
    }, silent = TRUE)
    if (inherits(dl, "try-error")) {
      message("File was too large for drive_download, using export link")
      if (nrow(get_result) < 1) {
        stop(paste0(ttype, " could not be downloaded"))
      }
      if (nrow(get_result) > 1) {
        warning("Multiple files detected - picking the first")
      }
      url = get_result$drive_resource[[1]]$exportLinks
      mime_ext = googledrive::drive_mime_type(type)
      url = url[[mime_ext]]
      if (!is.null(url)) {
        out = httr::GET(
          url,
          httr::write_disk(path, overwrite = TRUE),
          if (verbose) httr::progress())
        if (httr::status_code(out) >= 400) {
          stop(paste0(ttype, " could not be downloaded"))
        }
      } else {
        stop(paste0(ttype, " could not be downloaded"))
      }
    }
    return(dl)
  }

  get_result = googledrive::drive_get(id = id$id)

  pdf_file = tempfile(fileext = ".pdf")
  ###############################
  # Getting PDF
  ###############################
  if (verbose) {
    message(paste0("Downloading the PDF: ", pdf_file))
  }
  dl = download_wrapper(
    id = id,
    path = pdf_file,
    type = "pdf",
    get_result = get_result)

  return(pdf_file)
}
