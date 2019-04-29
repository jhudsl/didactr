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
#' @importFrom rmarkdown render pandoc_version
#' @importFrom rstudioapi viewer isAvailable
#' @importFrom utils browseURL
leanpub_render = function(
  md_file,
  output_options = list(self_contained = TRUE),
  ...) {
  if (rmarkdown::pandoc_version() <= package_version("1.20")) {
    stop(paste0("You must have pandoc version >= 1.20.  If ",
                "you have pandoc installed system-wide, please upgrade",
                ". Otherwise, upgrade RStudio version."))
  }
  md_file = normalizePath(md_file, mustWork = TRUE)
  tfile = basename(tempfile(fileext = ".md"))
  run_dir = dirname(md_file)
  tfile = file.path(run_dir, tfile)
  stub = sub("[.]md$", "", tfile)
  file.copy(md_file, tfile)
  on.exit({
    file.remove(tfile)
    file.remove(paste0(stub, ".html"))
    unlink(paste0(stub, "_files"), recursive = TRUE)
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
  xx = trimws(x)
  x = x[ !(xx %in% c("{format: png}", "{format: gif}"))]
  writeLines(x, con = tfile)
  res = tempfile(fileext = ".html")
  result = rmarkdown::render(
    input = tfile,
    output_dir = run_dir,
    output_format = "html_document",
    output_options = output_options,
    ...)
  file.copy(result, res)
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(res)
  } else {
    browseURL(res)
  }
  # Sys.sleep(time = 2)
  return(res)
}
