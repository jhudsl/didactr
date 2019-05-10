#' Is Google Language API Authorized
#'
#' @return A logical
#' @export
#'
#' @examples
#' is_language_auth()
is_language_auth = function() {
  inherits(googleAuthR::Authentication$public_fields$token, "Token")
}


#' @export
#' @rdname is_language_auth
didactr_gl_auth = function() {
  if (!is_language_auth()) {
    needed <- c("https://www.googleapis.com/auth/cloud-language",
                "https://www.googleapis.com/auth/cloud-platform")

    googleAuthR::gar_attach_auto_auth(needed,
                                      environment_var = "GL_AUTH")
  }
  is_language_auth()
}
