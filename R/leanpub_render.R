#' Render Leanpub document
#'
#' @param md_file Path to markdown file
#'
#' @return HTML output filename
#' @export
#' @importFrom rstudioapi viewer isAvailable
#' @importFrom utils browseURL
leanpub_render = function(md_file) {
  md_file = normalizePath(md_file, mustWork = TRUE)
  tfile = basename(tempfile())
  tfile = file.path(dirname(md_file), tfile)
  file.copy(md_file, tfile)
  on.exit({
    unlink(tfile)
  })
  x = readLines(tfile, warn = FALSE)
  image_lines = grep(x, pattern = "!\\[.*\\]\\((images.*)\\)")
  if (length(image_lines) > 0) {
    x[image_lines] = sub("\\(images", "(resources/images", x[image_lines])
  }
  writeLines(x, con = tfile)
  res = rmarkdown::render(
    input = tfile,
    output_dir = tempdir(),
    output_format = "html_document")
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(res)
  } else {
    browseURL(res)
  }
  return(res)
}
