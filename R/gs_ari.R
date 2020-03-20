
#' Convert Google Slides and notes to video with ari
#'
#' @param id Identifier of google slides presentation, passed to
#' \code{\link{gs_convert}}
#' @param script A vector strings that will be spoken over slides,
#' or a filename of the text.  Empty string are removed, please use
#' \code{;} if no words to be spoken.
#' @param output Output MP4 filename, optional
#' @param verbose Should diagnostics messages be printed
#' @param voice 	The Amazon Polly voice you want to use. See
#' \code{\link[aws.polly]{list_voices}} for more
#' information about what voices are available.
#' @param use_gs_pngs Use the PNGs that Google will export with
#' their slide deck.  If \code{FALSE}, must have
#' \code{animation} package installed.
#' @param ... Arguments passed to \code{\link{ari_spin}}
#'
#' @return A list of the images, script, and MP4 with the result.
#' @export
#' @importFrom ari ari_spin
#' @examples
#' \dontrun{
#' id = paste0("https://docs.google.com/presentation/d/",
#' "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8",
#' "/edit#slide=id.g154aa4fae2_0_58")
#' res = gs_ari(id)
#' }
gs_ari = function(
  id,
  output = NULL,
  script = NULL,
  verbose = TRUE,
  voice = "Joanna",
  use_gs_pngs = TRUE,
  ...) {

  res = gs_convert(
    id,
    verbose = verbose,
    use_gs_pngs = use_gs_pngs,
    PPTX = FALSE,
    use_gs_ids = FALSE)
  images = res$images
  if (is.null(script)) {
    script = res$script
  } else {
    if (length(script) == 1 & is.character(script)) {
      script = readLines(script, warn = FALSE)
      script = script[ !script %in% c("", " ")]
    }
  }

  if (is.null(output)) {
    output = tempfile(fileext = ".mp4")
  }

  result = ari_spin(
    images = images,
    paragraphs = script,
    output = output,
    verbose = verbose,
    voice = voice,
    ...)
  res$output = output
  res$result = result
  return(res)
}

#' @rdname gs_ari
#' @export
gs_ari_spin = gs_ari


#' @export
#' @rdname gs_ari
#' @note The \code{...} argument in \code{gs_tts} corresponds to
#' arguments passed to \code{\link[text2speech]{tts}}.
gs_tts = function(
  id,
  verbose = TRUE,
  voice = "Joanna",
  use_gs_pngs = TRUE,
  ...) {

  res = gs_convert(
    id,
    verbose = verbose,
    use_gs_pngs = use_gs_pngs,
    PPTX = FALSE,
    use_gs_ids = FALSE)
  # images = res$images
  paragraphs = res$script

  wav <- text2speech::tts(
    text = paragraphs,
    voice = voice,
    ...)
  tts_version = utils::packageVersion("text2speech")
  check_version = tts_version < package_version("0.2.4")
  if (check_version) {
    warning(paste0("text2speech version < 0.2.4 has a bug in ",
                   " ordering of wav output"))
  }
  res$wav = wav
  return(res)
}


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
#' @param voice 	The Amazon Polly voice you want to use. See
#' \code{\link[aws.polly]{list_voices}} for more
#' information about what voices are available.
#'
#' @return A list of results from \code{\link{gs_ari}} and
#' \code{\link{upload_video}}
#' @export
#' @importFrom tuber upload_video
gs_ari_upload = function(
  id,
  voice = "Joanna",
  ...,
  ffmpeg_opts = '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"',
  snippet = NULL, status = NULL,
  open_url = TRUE) {
  res = gs_ari(
    id,
    voice = voice,
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


