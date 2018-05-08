
#' Get Microsoft Text To Speech (TTS) or Cognitive
#' Services Token from API Key
#'
#' @param api_key Microsoft Cognitive Services API key, if token is not
#' provided.
#' @param script A character vector of lines to be spoken
#' @param token An authentication token, base-64 encoded usually from
#' \code{\link{get_ms_tts_token}}.  If not provided, will be created from
#' \code{\link{get_ms_tts_token}}
#' @param gender Sex of the Speaker
#' @param language Language to be spoken,
#' must be from \code{\link{ms_language_codes}}
#' @param output_format Format of the output, see
#' \url{https://docs.microsoft.com/en-us/azure/cognitive-services/speech/api-reference-rest/bingvoiceoutput}
#' for more information
#' @param ... Additional arguments to send to \code{\link{POST}}
#'
#' @return A list of the request, content, token, and `SSML`.
#' @note The
#' content is likely in a binary format and the output depends on the
#' `output_format` chosen.  For example, if the `output_format` is an `MP3`,
#' then see below example
#'
#' @examples \dontrun{
#' if (have_ms_tts_key()) {
#' res = ms_synthesize(script = "hey",
#' output_format = "audio-16khz-128kbitrate-mono-mp3")
#' tmp <- tempfile()
#' writeBin(out, con = tmp)
#' mp3 = tuneR::readMP3(tmp)
#' }
#'
#' }
#' @export
#' @importFrom httr POST add_headers stop_for_status content content_type
ms_synthesize = function(
  script,
  token = NULL,
  api_key = NULL,
  gender = "Female",
  language = "en-US",
  output_format = c( "raw-16khz-16bit-mono-pcm" ,
                     "ssml-16khz-16bit-mono-tts",
                    "audio-16khz-16kbps-mono-siren",
                    "riff-16khz-16kbps-mono-siren",
                    "riff-16khz-16bit-mono-pcm",
                    "audio-16khz-128kbitrate-mono-mp3",
                    "audio-16khz-64kbitrate-mono-mp3",
                    "audio-16khz-32kbitrate-mono-mp3"),
  ...
  ){
  locales = ms_locales()
  n_locales = names(locales)
  stopifnot(length(language) == 1)
  if (!(language %in% n_locales)) {
    stop(paste0("Lanuage ", language, " not in locales"))
  }
  locales = locales[[language]]
  n_locales = names(locales)
  stopifnot(length(gender) == 1)
  if (!(gender %in% n_locales)) {
    stop(paste0("Gender ", gender, " not in locales for this language"))
  }
  xname = locales[[gender]]


  synth_url = paste0(
    'https://speech.platform.bing.com/',
    'synthesize')
  if (is.null(token)) {
    token = get_ms_tts_token(api_key = api_key)$token
  }

  auth_hdr = add_headers(
    "Authorization" = token)
  output_format = match.arg(output_format)

  fmt_hdr = add_headers(
    "X-Microsoft-OutputFormat" = output_format)

  ctype = content_type("application/ssml+xml")


  ssml = c(paste0(
    "<speak version='1.0' ", "xml:lang='",
    language, "'>"),
    paste0("<voice xml:lang='", language ,"'",
           " xml:gender='", gender, "'"),
    paste0(" name='", xname, "'"),
    ">",
    script, "</voice>",
    "</speak>")
  ssml = paste(ssml, collapse = "")

  if (nchar(ssml) > 1024) {
    cat(ssml)
    stop("Need smaller script! SSML is over 1024 characters")
  }
  res = POST(synth_url,
             body = ssml,
             auth_hdr, fmt_hdr, ctype, auth_hdr,
             ...)
  stop_for_status(res)
  out = content(res)
  class(token) = "token"

  L = list(
    request = res,
    ssml = ssml,
    content = out,
    token = token)
  return(L)

}
