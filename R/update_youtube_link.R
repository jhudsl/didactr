#' Update Youtube Link
#'
#' @param course_status output from \code{\link{check_course}}
#'
#' @return updated manuscript files
#' @export
#'
update_youtube_link <- function(course_status = NULL){
  df = course_status$course_summary
  paths = course_status$paths

  if(file.exists(file.path(paths$met_path,"youtube_uploads.rds"))){
    readRDS(file.path(paths$met_path,"youtube_uploads.rds"))
  }
  sapply(df$lesson,
         function(x) {
           ## get vids for this lesson
           vids = youtube_uploads %>%
             mutate(les = sub("[.]mp4$", "", basename(file)) ) %>%
             filter(les == x) %>%
             arrange(desc(time_published))
          ## update if no link exists
          ## or if more recent upload has occurred
           if(is.na(df$yt_md_link[df$lesson == x]) | nrow(vids)>0 & vids$url[1] != df$yt_md_link[df$lesson == x]){
             message(paste0("updating youtube link in manuscript file: ", x))
             t  <- readLines(df$md_file[df$lesson == x])
             # identify which link to edit for the video
             line <- grep(pattern = "^!\\[.+\\]\\((?!\\.png)\\)|!\\[.+\\]\\(.+[^.png]\\)|^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)",t,perl=TRUE)
             t[line]<-gsub("\\(.+\\)|\\(\\)",paste0("(",vids$url[1],")"),t[line])
             writeLines(t, con=df$md_file[df$lesson == x])
           }
         })
  ret = check_course(course_dir = course_status$course_dir,
                     save_metrics = course_status$save_metrics)
  return(ret)
}
