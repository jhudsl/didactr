#' Check Course File Structure
#'
#' @param course_dir directory with course materials
#'
#' @return A list of the paths necessary for the course
#' @export
#' @importFrom googledrive drive_get
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_sub
#' @import dplyr
#'
check_structure = function(course_dir = ".") {
  course_dir = normalizePath(course_dir)
  # set paths
  scr_path = file.path(course_dir, "scripts")
  man_path = file.path(course_dir, "manuscript")
  res_path = file.path(man_path, "resources")
  vid_path = file.path(res_path, "videos")
  img_path = file.path(res_path, "images")
  met_path = file.path(course_dir, "metrics")

  paths = list(scr_path, res_path, man_path, img_path, vid_path, met_path)
  names(paths) = c("scr_path", "res_path", "man_path",
                   "img_path", "vid_path", "met_path")

  # function to check if correct directories exist
  check_dir <- function(path){
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      message(paste0("Creating missing ", path, " directory"))
    }
  }

  sapply(paths, check_dir)
  return(paths)
}
