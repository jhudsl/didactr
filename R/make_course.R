#' Check for expected file structure and create missing
#'
#' @param course_dir directory with course materials
#' @param book_txt A text file with names of markdown
#'  files and course names
#' @param verbose print diagnostic messages
#' @param warn_book_exists Should a warning be sent if \code{Book.txt}
#' already exists.
#'
#' @return A list of the paths necessary for the course
#' @export
#'
#' @examples
#' course_dir = tempfile()
#' dir.create(course_dir)
#' make_course(course_dir)
#' @rdname check_structure
make_course = function(
  course_dir = ".",
  book_txt = NULL,
  verbose = TRUE,
  warn_book_exists = TRUE) {

  course_dir = normalizePath(course_dir, mustWork = TRUE)
  met_path = file.path(course_dir, "metrics")

  scr_path = file.path(course_dir, "scripts")
  man_path = file.path(course_dir, "manuscript")
  res_path = file.path(man_path, "resources")
  vid_path = file.path(res_path, "videos")
  img_path = file.path(res_path, "images")

  paths = list(
    course_dir = course_dir,
    scr_path = scr_path,
    res_path = res_path,
    man_path = man_path,
    img_path = img_path,
    vid_path = vid_path,
    met_path = met_path)

  # function to check if correct directories exist
  check_structure <- function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      if (verbose) {
        message(paste0("Creating missing ", path, " directory"))
      }
    }
  }



  dirs_created <- sapply(paths, check_structure)

  out_book_txt = file.path(man_path, "Book.txt")
  if (!is.null(book_txt)) {
    if (file.exists(book_txt)) {
      if (!file.exists(out_book_txt)) {
        file.copy(book_txt, out_book_txt)
      } else {
        if (warn_book_exists) {
          warning(paste0(out_book_txt, " already exists and ",
                         "book_txt specified, not overwriting!"))
        }
      }
    }
  }
  if (!file.exists(out_book_txt)) {
    writeLines("", con = out_book_txt)
  }
  paths$book_txt = out_book_txt
  paths$course_name = basename(course_dir)
  class(paths) = "structure_check"
  return(paths)
}

#' @rdname check_structure
#' @export
check_structure = make_course
