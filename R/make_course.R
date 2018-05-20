#' Check for expected file structure and create missing
#'
#' @param course_dir directory with course materials
#'
#' @return A data frame of the checked course.
#' @export
#' @importFrom ari ari_spin
#'

make_course = function(course_dir = NULL) {

scr_path = file.path("scripts")
res_path = file.path("manuscript", "resources")
man_path = file.path("manuscript")
vid_path = file.path("manuscript", "resources", "videos")
img_path = file.path("manuscript", "resources", "images")

paths = list(scr_path, res_path, man_path, img_path, vid_path)

# function to check if correct directories exist
check_structure <- function(path){
  if(!dir.exists(path)){
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message(paste0("Creating missing ", path, " directory"))
  }
}

dirs_created<-sapply(paths, check_structure)
}
