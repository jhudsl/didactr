
#' Starting a Course, Light wrapper for \code{\link{make_course}}
#'
#' @param course_name Name of the course.  Any spaces will be set to
#' underscores
#' @param root_path The directory to put the course into.
#' @param book_txt A text file with names of markdown
#'  files and course names
#' @param verbose print diagnostic messages
#' @param ... additional argument to pass to \code{\link{make_lesson}}
#'
#' @return The output of \code{\link{make_course}}.
#' @export
#' @examples
#' root_path = tempfile()
#' course_name = "test this out"
#' sc = start_course(course_name, root_path)
#'
#'
#' root_path = tempfile()
#' course_name = "test this out2"
#' book_txt =  system.file("extdata", "Book.txt", package = "didactr")
#' sc = start_course(course_name, root_path, book_txt = book_txt)
#' readLines(sc$book_txt)
#' md_files = list.files(sc$man_path, full.names = TRUE)
#' md_files
#' readLines(md_files[1])
start_course = function(course_name, root_path = ".",
                        book_txt = NULL,
                        verbose = TRUE,
                        ...){
  course_name = sub(" ", "_", course_name)

  course_dir = file.path(root_path, course_name)
  dir.create(course_dir, showWarnings = FALSE, recursive = TRUE)

  res = make_course(course_dir = course_dir, book_txt = book_txt,
                    verbose = verbose)
  book_txt = readLines(res$book_txt, warn = FALSE)
  book_txt = trimws(book_txt)
  book_txt = book_txt[ book_txt != ""]

  if (length(book_txt) > 0) {
    lessons = make_lessons_from_book(
      course_dir = res$course_dir,
      verbose = verbose,
      ...)
    res$lessons = lessons
  }

  return(res)
}
