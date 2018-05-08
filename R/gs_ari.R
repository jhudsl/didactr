
#' Convert Google Slides and notes to video with ari
#'
#' @param id Identifier of google slides presentation, passed to
#' \code{\link{gs_convert}}
#' @param output Output MP4 filename, optional
#' @param verbose Should diagnostics messages be printed
#' @param ... Arguments passed to \code{\link{ari_spin}}
#'
#' @return A list of the images, script, and MP4 with the result.
#' @export
#' @importFrom ari ari_spin
gs_ari = function(id, output = NULL,
                  verbose = TRUE, ...) {

  res = gs_convert(id, verbose = verbose)
  images = res$images
  script = res$script

  if (is.null(output)) {
    output = tempfile(fileext = ".mp4")
  }

  result = ari_spin(
    images = images,
    paragraphs = script,
    output = output,
    verbose = verbose,
    ...)
  res$output = output
  res$result = result
  return(res)
}
