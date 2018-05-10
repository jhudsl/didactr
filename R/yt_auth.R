#' Authorize with Youtube
#'
#' @param json Takes a Google Authentication JSON and
#' passes it to \code{\link{yt_oauth}}
#' @param ... Additional arguments to send to \code{\link{yt_oauth}} other
#' than `app_id` and `app_secret`
#'
#' @importFrom tuber yt_oauth
#' @importFrom jsonlite fromJSON
#'
#' @return The result from \code{\link{yt_oauth}}, which sets the
#' Google Token
#' @export
yt_auth = function(json, ...) {
  if (file.exists(json)) {
    json = normalizePath(json)
  }
  out = jsonlite::fromJSON(json)
  tuber::yt_oauth(app_id = out$installed$client_id,
           app_secret = out$installed$client_secret,
           ...)
}
