#' Get Slide ID from URL
#'
#' @param x URL of slide
#'
#' @return A character vector
#' @export
#'
#' @examples
#' x = paste0("https://docs.google.com/presentation/d/",
#' "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8",
#' "/edit#slide=id.g154aa4fae2_0_58")
#' get_slide_id(x)
get_slide_id = function(x) {
  x = sub(".*presentation/", "", x)
  x = sub("/d/e", "/d", x) # if you publish by accident
  x = sub("^(d|e)/", "", x)
  x = strsplit(x, "/")[[1]]
  x = x[ !grepl("^(edit|pub|export|png)", x)]
  x = x[ nchar(x) > 5]
  x
}

#' @export
#' @rdname get_slide_id
make_slide_url = function(x) {
  x = get_slide_id(x)
  x = paste0("https://docs.google.com/presentation/d/",x)
  x
}
