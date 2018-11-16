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
