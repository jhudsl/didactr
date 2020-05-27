#' Convert Google Slides to PNGs and a script
#'
#' @param id File id passed to \code{\link{drive_download}} after passing
#' through \code{\link{as_id}}
#' @param verbose logical, indicating whether to print informative messages,
#' passed to \code{\link{drive_download}}
#' @param PPTX If \code{TRUE}, then the PowerPoint is downloaded as well.
#' If this is \code{FALSE}, then the script will not be generated.
#' @param auto_stub Should the prefix name be taken from
#' the Google slide name?  If so, `stub` argument will
#' be overridden in \code{\link{pdf_to_images}}
#' @param use_gs_ids use Google slide identifiers for the naming
#' @param ... Additional options to send to \code{\link{pdf_to_images}}
#' if \code{use_gs_pngs = FALSE}
#' @param use_gs_pngs Use the PNGs that Google will export with
#' their slide deck.  If \code{FALSE}, must have
#' \code{animation} package installed.
#' @return A list of the images and the notes for each script
#' @export
#'
#' @importFrom googledrive drive_download as_id drive_get
#' @examples \dontrun{
#' id = "1XoRj0pwaLI34XKZ7TljVeDHu-tbgGmXRmQa528JIwmw"
#' PPTX = FALSE
#' auto_stub = TRUE
#' res = gs_convert(id, use_gs_ids = TRUE, PPTX = FALSE)
#' res = gs_convert(id, use_gs_pngs = FALSE)
#' res = gs_convert(id, use_gs_pngs = TRUE, PPTX = FALSE)
#' res = gs_convert(id, use_gs_pngs = TRUE, use_gs_ids = TRUE,
#' PPTX = FALSE)
#' }
gs_convert = function(
  id, verbose = TRUE,
  PPTX = TRUE,
  auto_stub = TRUE,
  use_gs_ids = FALSE,
  use_gs_pngs = TRUE,
  ...) {

  check_didactr_auth()
  if (is.character(id)) {
    id = get_slide_id(id)
  }
  id = googledrive::as_id(id)
  tdir = tempfile()
  dir.create(tdir)
  pdf_file = tempfile(fileext = ".pdf", tmpdir = tdir)

  args = list(...)
  if (auto_stub) {
    if ("stub" %in% names(args)) {
      warning(paste0(
        "Stub was passed into gs_convert, but autostub",
        "is TRUE, not using your stub"))
    }
    lesson_name = googledrive::drive_get(id = as_id(id))$name
    lesson_name = paste0(lesson_name, "-%05d")
    args$stub = lesson_name
  }

  ###############################
  # Getting PDF
  ###############################
  if (verbose) {
    message(paste0("Downloading the PDF: ", pdf_file))
  }
  dl = drive_download(
    id,
    path = pdf_file,
    type = "pdf")



  if (PPTX) {
    pptx_file = tempfile(fileext = ".pptx", tmpdir = tdir)
    if (verbose) {
      message(paste0("Downloading the PPTX: ", pptx_file))
    }
    pptx_dl = googledrive::drive_download(id,
                                          path = pptx_file,
                                          type = "pptx")
    if (verbose) {
      message("Getting Notes from PPTX")
    }
    script = pptx_notes(pptx_file)
  } else {
    slide_df = gs_slide_df(id)
    script = notes_from_slide_output(slide_df)
    pptx_file = NULL
  }

  ##################################
  # Get the PNGs
  ##################################
  slide_df = NULL
  error_png_result = FALSE
  if (use_gs_pngs) {
    slide_df = gs_slide_df(id)
    download_png_result = try({
      pngs = download_png_urls(slide_df$png_url)
    }, silent = TRUE)
    error_png_result = inherits(download_png_result, "try-error")
    if (error_png_result) {
      warning(paste0("PNGs from Google could not be used, ",
                     "link sharing is likely not on.  Using conversion of ",
                     "PDF.  To suppress, set use_gs_pngs = FALSE"))
    }
  }
  if (!use_gs_pngs | error_png_result) {
    args$pdf_file = pdf_file
    if (verbose) {
      message("Converting PDF to Images")
    }
    if (verbose) {
      message(paste0("Stub for Conversion is:", args$stub))
    }
    pngs = do.call(pdf_to_images, args)
  }

  if (use_gs_ids) {
    slide_df = rename_pngs_to_gs_ids(id = id, pngs = pngs)
    same_length = attributes(slide_df)$same_length
    if (is.null(same_length)) {
      same_length = FALSE
    }
    if (same_length) {
      pngs = slide_df$png
    }
  }
  L = list(images = pngs)
  L$pdf_file = pdf_file
  L$slide_df = slide_df
  L$script = script
  L$pptx_file = pptx_file
  return(L)
}


#' Rename set of PNGs to Google Slide IDs
#'
#' @param id File id passed to \code{\link{drive_download}} after passing
#' through \code{\link{as_id}}
#' @param pngs Vector of PNG filenames
#'
#' @return A \code{data.frame}, similar to output of \code{\link{gs_slide_df}}
#' @export
rename_pngs_to_gs_ids = function(id, pngs) {
  slide_df = gs_slide_df(id = id)

  slide_ids = slide_df$objectId
  any_dups = anyDuplicated(slide_ids) > 0
  if (length(pngs) != length(slide_ids) || any_dups) {
    warning(paste0("Not renaming slides - ",
                   "different length as API output!"))
    attr(slide_df, "same_length") = FALSE
  } else {
    outfiles = file.path(dirname(pngs), paste0(slide_ids, ".png"))
    copy_result = file.rename(pngs, outfiles)
    if (!all(copy_result)) {
      warning(paste0(
        "Renaming didn't correctly work with use_gs_ids!",
        " Results probably off"))
    }
    slide_df$old_png = pngs
    slide_df$png = outfiles
    pngs = outfiles
    attr(slide_df, "same_length") = TRUE
  }
  return(slide_df)
}
