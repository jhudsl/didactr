
mooc_app = function() {
  httr::oauth_app(
    appname = "mooc-generator",
    key = "299711289983-km4ktlcq0mc81e5trravudt8sqcqfl3k.apps.googleusercontent.com",
    secret = "gfA5FKtuKbYP4eOFDrIFD0yt",
    redirect_uri = "http://localhost:1410/"
  )
}

#' Authorize Application for MOOC
#'
#' @param cache A logical value or a string. \code{TRUE} means to cache
#' using the default cache file \code{.httr-oauth}
#' @param use_oob use a local webserver for the OAuth dance
#'
#' @return A list with the token and the saved file with the token
#' @export
#'
#' @importFrom httr oauth_endpoints oauth2.0_token
#' @importFrom googledrive drive_auth
#' @examples \dontrun{
#' authorize_mooc_app()
#' }
authorize_mooc_app = function(cache = FALSE, use_oob = FALSE) {

  cred <- httr::oauth2.0_token(
    endpoint = httr::oauth_endpoints("google"),
    app = mooc_app(),
    scope = c("https://www.googleapis.com/auth/drive",
              "https://www.googleapis.com/auth/youtube.force-ssl",
              "https://www.googleapis.com/auth/presentations"),
    cache = cache,
    use_oob = use_oob)

  # for tuber
  options(google_token = cred)
  tfile = tempfile(fileext = ".rds")
  saveRDS(cred, tfile)
  googledrive::drive_auth(oauth_token = tfile)
  return(list(token = cred, token_file = tfile))
}


