#' Render Leanpub document
#'
#' @param md_file Path to markdown file
#' @param output_options arguments to send to \code{\link{render}}
#' @param ... additional arguments to send to \code{\link{render}}
#' other than \code{input}, `output_dir`, and `output_format`,
#' and `output_options`
#'
#' @return HTML output filename
#' @export
#' @importFrom rmarkdown render
#' @importFrom rstudioapi viewer isAvailable
#' @importFrom utils browseURL
leanpub_render = function(
  md_file,
  output_options = list(self_contained = FALSE),
  ...) {
  md_file = normalizePath(md_file, mustWork = TRUE)
  tfile = basename(tempfile(fileext = ".md"))
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
  image_lines = grep(x, pattern = "!\\[.*\\]\\((http*)\\)")
  if (length(image_lines) > 0) {
    x[image_lines] = sub("edit#slide=id.", "export/png?id=", x[image_lines],
                         fixed = TRUE)
  }
  writeLines(x, con = tfile)
  res = rmarkdown::render(
    input = tfile,
    output_dir = tempdir(),
    output_format = "html_document",
    output_options = output_options,
    ...)
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(res)
  } else {
    browseURL(res)
  }
  return(res)
}
