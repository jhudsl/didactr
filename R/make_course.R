#' Check for expected file structure and create missing
#'
#' @param course_dir directory with course materials
#' @param verbose print diagnostic messages
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
  verbose = TRUE) {

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
  return(paths)
}

#' @rdname check_structure
#' @export
check_structure = make_course
