
#' Starting a Course, Light wrapper for \code{\link{make_course}}
#'
#' @param course_name Name of the course.  Any spaces will be set to
#' underscores
#' @param root_path The directory to put the course into.
#'
#' @return The output of \code{\link{make_course}}.
#' @export
#' @examples
#' root_path = tempfile()
#' course_name = "test this out"
#' sc = start_course(course_name, root_path)
start_course = function(course_name, root_path = "."){
  course_name = sub(" ", "_", course_name)

  course_dir = file.path(root_path, course_name)
  dir.create(course_dir, showWarnings = FALSE, recursive = TRUE)

  res = make_course(course_dir = course_dir)
  book_txt = res$book_txt

  return(res)
}
