#' Get Notes from a PowerPoint (usually from Google Slides)
#'
#' @param file Character. Path for `PPTX` file
#'
#' @return Either a character vector or \code{NULL}
#' @export
#'
#' @importFrom utils unzip
#' @examples
#' ex_file = system.file("extdata", "example.pptx", package = "didactr")
#' pptx_notes(ex_file)
pptx_notes = function(file) {
  tdir = tempfile()
  dir.create(tdir)
  res = unzip(file, exdir = tdir)
  note_dir = file.path(tdir, "ppt", "notesSlides")
  notes = list.files(path = note_dir, pattern = "[.]xml$",
                     full.names = TRUE)
  if (length(notes) > 0) {
    res = sapply(notes, xml_notes)
    names(res) = basename(notes)

    return(res)
  } else {
    return(NULL)
  }
}
