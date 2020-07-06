pptx_reorder_xml = function(files) {
  if (length(files) == 0) {
    return(files)
  }
  nums = basename(files)
  # nums = gsub(pattern = paste0(pattern, "(\\d*)[.]xml"),
  #             replacement = "\\1", nums)
  nums = sub("[[:alpha:]]*(\\d.*)[.].*", "\\1", nums)
  nums = as.numeric(nums)
  if (any(is.na(nums))) {
    warning(paste0("Trying to parse set of files (example: ", files[1],
                   ") from PPTX, failed"))
    return(files)
  }
  files = files[order(nums)]
}

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
    notes = pptx_reorder_xml(notes)
    res = sapply(notes, xml_notes)
    names(res) = basename(notes)
    return(res)
  } else {
    return(NULL)
  }
}
