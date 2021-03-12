#' Summary of course check
#'
#' @param object An object of class \code{course_check}
#' @param ... Not used
#'
#' @return A list of data frames of various checks
#' @export
#'
#' @examples
#' root_path = tempfile()
#' course_name = "test this out2"
#' book_txt =  system.file("extdata", "Book.txt", package = "didactr")
#' sc = create_course(course_name, root_path, book_txt = book_txt)
#' course_dir = sc$course_dir
#' object = check_course(sc$course_dir,
#' require_authorization = FALSE)
#' summary(object)
summary.course_check = function(object, ...) {

  all_images_exist = scr_png_match = NULL
  rm(list= c("all_images_exist", "scr_png_match"))
  cs = object$course_summary
  bad_images = cs %>%
    filter(!all_images_exist)
  if (nrow(bad_images) > 0) {
    cat(
      paste0(
        "Images referenced in lesson do not exist in images directory: ",
        paste(bad_images$lesson, collapse = ", "),
        "\n")
    )
  }
  bad_scr = cs %>%
    filter(!scr_png_match)
  if (nrow(bad_scr) > 0) {
    cat(
      paste0(
        "Script length and number of images do not agree: ",
        paste(bad_scr$lesson, collapse = ", "),
        "\n")
    )
  }
  # invisible(
  #   list(image_check_fail = bad_images,
  #      script_length_fail = bad_scr
  #      )
  # )
  invisible(NULL)
}
