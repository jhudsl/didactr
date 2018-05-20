#' Update Youtube Link
#'
#' @param course_status output from \code{\link{check_course}}
#'
#' @return updated manuscript files
#' @export
#'
update_youtube_link <- function(course_status = NULL){
  df = course_status$course_summary
  if(file.exists(file.path(met_path,"youtube_uploads.rda"))){
    load(file.path(met_path,"youtube_uploads.rda"))
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
             line <- grep(pattern = "!\\[.+\\]\\(.+[^.png]\\)|!\\[.+\\]\\(.+youtu.+\\)|!\\[.+\\]\\()",t)
             t[line]<-gsub("\\(.+\\)|\\(\\)",paste0("(",vids$url[1],")"),t[line])
             writeLines(t, con=df$md_file[df$lesson == x])
           }
         })
}
