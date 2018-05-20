#' Check Course File Structure
#'
#' @param course_dir directory with course materials
#'
#' @return A data frame of the checked course.
#' @export
#' @importFrom googledrive drive_get
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_sub
#' @import dplyr
#'
check_structure = function(course_dir = NULL) {
  setwd(normalizePath(course_dir))
  # set paths
  scr_path = file.path("scripts")
  res_path = file.path("manuscript", "resources")
  man_path = file.path("manuscript")
  vid_path = file.path("manuscript", "resources", "videos")
  img_path = file.path("manuscript", "resources", "images")
  met_path = file.path("metrics")

  paths = list(scr_path, res_path, man_path, img_path, vid_path, met_path)

  # function to check if correct directories exist
  check_dir <- function(path){
    if(!dir.exists(path)){
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      message(paste0("Creating missing ", path, " directory"))
    }
  }

  dirs_created<-sapply(paths, check_dir)
}
