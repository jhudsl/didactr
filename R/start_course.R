
#' Starting a Course, Light wrapper for \code{\link{make_course}}
#'
#' @param course_name Name of the course.  Any spaces will be set to
#' underscores
#' @param root_path The directory to put the course into.
#' @param book_txt A text file with names of markdown
#'  files and course names
#' @param verbose print diagnostic messages
#' @param open 	If \code{TRUE}, activates the new project:
#' If RStudio desktop, the package is opened in a new session.
#' Otherwise, the working directory and active project is changed.
#' @param rstudio If \code{TRUE}, calls \code{use_rstudio()} to
#' make the new package or project into an RStudio Project.
#' @param ... additional argument to pass to \code{\link{make_lesson}}
#'
#' @importFrom usethis proj_set use_rstudio proj_activate
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
                        rstudio = FALSE,
                        open = FALSE,
                        ...){
  course_name = gsub(" ", "_", course_name)

  course_dir = file.path(root_path, course_name)
  dir.create(course_dir, showWarnings = FALSE, recursive = TRUE)

  old_project <- usethis::proj_set(course_dir, force = TRUE)
  on.exit(usethis::proj_set(old_project), add = TRUE)

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

  if (rstudio) {
    usethis::use_rstudio()
  }

  if (open) {
    if (usethis::proj_activate(course_dir)) {
      # Working directory/active project changed; so don't undo on exit
      on.exit()
    }
  }
  return(res)
}

#' @export
#' @rdname start_course
create_course = start_course
