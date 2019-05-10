#' Convert PDF to Other Images using ImageMagick
#'
#' @param pdf_file Filename of PDF file
#' @param extra.opts Extra options passed to \code{animation::im.convert}
#' @param stub prefix of the output files, can include C-style printing of
#' digits and such
#' @param output_type Type of output, such as `png` or `jpg`
#' @param out_dir Output directory.  If `NULL`, then a temporary directory
#' will be used.  If files of the same name are in the `out_dir`, these
#' will be overwritten
#' @param convert converter passed to \code{animation::im.convert}
#'
#' @return A character vector of filenames
#' @export
#'
#' @examples \dontrun{
#' ex_file = system.file("extdata", "example.pdf", package = "didactr")
#' res = pdf_to_images(ex_file)
#' }
pdf_to_images = function(
  pdf_file,
  out_dir = NULL,
  extra.opts = "-density 300 -quality 100" ,
  stub = "img_%04d",
  output_type = "png",
  convert = "convert"){
  Sys.setenv(MAGICK_THREAD_LIMIT = 1);

  pdf_file = normalizePath(pdf_file, mustWork = TRUE)

  ######################################
  # Turn off the animation options
  ######################################
  if (!requireNamespace("animation", quietly = TRUE)) {
    stop(
      paste0("Package \"animation\" needed for pdf_to_images to work. ",
             "Please install it."),
      call. = FALSE)
  }
  aniopts = animation::ani.options()
  animation::ani.options(autobrowse = FALSE)
  animation::ani.options(interval = 0)

  if (!is.null(out_dir)) {
    out_dir = normalizePath(out_dir)
  }
  # change directory back and then set the options back
  owd = getwd()
  on.exit({
    setwd(owd)
    animation::ani.options(aniopts)
  })
  tdir = tempfile()
  dir.create(tdir, showWarnings = FALSE)
  png_types = paste0(stub, ".", output_type)
  setwd(tdir)
  # convert it
  animation::im.convert(pdf_file,
                        output = png_types,
                        extra.opts = extra.opts,
                        convert = convert)
  results = list.files(path = tdir,
                       full.names = TRUE)
  if (!is.null(out_dir)) {
    file.copy(results, to = out_dir, overwrite = TRUE)
    results = file.path(out_dir, basename(results))
  }
  ######################################
  # Reinstate
  #######################################
  results
}
