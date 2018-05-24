#' Upload videos to YouTube
#'
#' @param course_status object output from \code{\link{check_course}}
#' @param Course Course to be used in title of youtube video
#' @param josn Link to json file with credentials
#'
#' @return youtube_uploads data frame with metrics from upload
#' @export
#' @importFrom stringr str_to_title
#' @importFrom dplyr data_frame
#'

vids_to_youtube <- function(course_status = NULL, Course = NULL,
                            json = NULL){
  if (!is.null(json)) {
    yt_auth(json = json)
  }
  df = course_status$course_summary
  ## if no video link in df
  ## or if video link in df not the most updated
  sapply(df$vid_file,
         function(x) {
           ## decide if video needs to be uploaded
           ## john when you look at this code, know that i'm sorry
           ## it was a saturday morning
           ## hopefully I delete this and improve before you ever see this note
           if(file.exists(file.path(met_path,"youtube_uploads.rda"))){
             load(file.path(met_path,"youtube_uploads.rda"))
              vids = youtube_uploads %>%
               filter(lesson == df$lesson[df$vid_file==x]) %>%
               arrange(desc(time_published))
             if(nrow(vids) > 0){
               make_video <- vids$time_published[vids$file == basename(x)] < df$mod_time_vid[df$vid_file == x]
             }else{
               vids = data_frame(lesson=df$lesson[df$vid_file==x], id = NA, time_published = NA)
               make_video = TRUE
             }
           }else{
               vids = data_frame(lesson=df$lesson[df$vid_file==x], id = NA, time_published = NA)
               make_video = TRUE
             }
           if(!df$has_vid_link[df$vid_file==x]|make_video){
             # get info from file for video title
             lesson = sub("[.]mp4$", "", basename(x))
             lesson_name = sub("[.]mp4$", "", basename(x)) %>%
               sub("\\d+_","",.) %>%
               gsub("_"," ",. ) %>%
               stringr::str_to_title(.)
             title = paste0(Course,": ", lesson_name)
             file = x
             ## upload video to youtube
             message(paste0("uploading video to youtube for: ", lesson_name))
             up = upload_video(file = file,
                               snippet = list(title = title),
                               status= list(privacyStatus = "unlisted",
                                            license = "creativeCommon"))

             #update uploaded videos data frame
             youtube_uploads <- update_youtube(up, file=file, lesson=lesson)
           }})
  ret = check_course(course_dir = course_status$course_dir)
  return(ret)
}
