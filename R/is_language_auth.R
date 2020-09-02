#' Is Google Language API Authorized
#'
#' @return A logical
#' @importFrom utils packageVersion
#' @export
#'
#' @examples
#' is_language_auth()
is_language_auth = function() {
  if (!requireNamespace("googleAuthR", quietly = FALSE)) {
    stop("googleAuthR required for is_language_auth")
  }
  if (utils::packageVersion("googleAuthR") >= package_version("1.0.0")) {
    # googleAuthR:::.auth$auth_active
    object = get(".auth", envir = asNamespace("googleAuthR"))
    object = object$cred
    # inherits(googleAuthR:::.auth$cred, "Token")
  } else {
    object = get("Authentication", envir = asNamespace("googleAuthR"))
    object = object$public_fields$token
    # inherits(googleAuthR::Authentication$public_fields$token, "Token")
  }
  inherits(object, "Token")

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
