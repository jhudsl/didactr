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

#' @rdname get_slide_id
#' @export
#' @examples
#' x = "https://drive.google.com/drive/folders/1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC?usp=sharing"
#' get_folder_id(x)
#' x = "1pXBQQdd1peI56GtQT-jEZ59xSmhqQlFC"
#' get_folder_id(x)
get_folder_id = function(x) {
  res = httr::parse_url(x)
  x = res$path
  x = sub(".*folders/", "", x)
  x = sub("[?].*", "", x)
  x = x[ nchar(x) > 5]
  x
}
