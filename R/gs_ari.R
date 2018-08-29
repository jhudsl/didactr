
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
gs_ari = function(
  id, output = NULL,
  verbose = TRUE, ...) {

  res = gs_convert(id,
                   verbose = verbose,
                   PPTX = TRUE,
                   use_gs_ids = FALSE)
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

#' @rdname gs_ari
#' @export
gs_ari_spin = gs_ari


#' Convert Google Slides to Video and Upload to Youtube
#'
#' @param id Identifier of google slides presentation, passed to
#' \code{\link{gs_convert}}
#' @param ... Arguments passed to \code{\link{gs_ari}}
#' @param ffmpeg_opts options passed to \code{\link{ari_spin}}
#' @param status status list passed to \code{\link{upload_video}}
#' @param snippet snippet list passed to \code{\link{upload_video}}
#' @param open_url Should YouTube URL be opened?  Passed to
#' \code{\link{upload_video}}
#'
#' @return A list of results from \code{\link{gs_ari}} and
#' \code{\link{upload_video}}
#' @export
#' @importFrom tuber upload_video
gs_ari_upload = function(
  id, ...,
  ffmpeg_opts = '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"',
  snippet = NULL, status = NULL,
  open_url = TRUE) {
  res = gs_ari(
    id,
    ...,
    cleanup = FALSE,
    ffmpeg_opts = ffmpeg_opts
  )

  ####### yt_authentication done here #####

  vid = tuber::upload_video(file = res$output,
                            status = status,
                            snippet = snippet,
                            open_url = open_url)
  res$video = vid
  return(res)
}
