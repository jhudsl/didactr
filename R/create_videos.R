#' Create video from script and PNGs
#'
#' @param course_status output from \code{\link{check_course}}
#'
#' @return created mp4s
#' @importFrom ari ari_spin
#' @export
#'

create_videos <- function(course_status = NULL){
  df = course_status$course_summary
  ## create video: ari_spin()
  ## if no video link in df
  ## OR if pngs are more recent than youtube video
  sapply(df$lesson,
         function(x) {
           if(!df$has_vid_file[df$lesson==x] | df$vid_more_recent[df$lesson==x]){
             files <- grep("[.]png", list.files(file.path(img_path, x), full.names = TRUE),
                           value = TRUE)

             ## reorder image files if they end in -1 rather than -01
             if(length(grep("[0][0-9].png",files[1]))<1){
               files1 <- grep("[.]png", list.files(file.path(img_path,x),
                                                   pattern = "-[0123456789].png",
                                                   full.names = TRUE),
                              value = TRUE)
               files2 <- grep("[.]png", list.files(paste0('manuscript/resources/images/',x),
                                                   pattern = "-[0123456789][0123456789].png",
                                                   full.names = TRUE),
                              value = TRUE)
               files <- c(files1, files2)
             }

             para = readLines(file.path(scr_path, paste0(x, '_script.md')), warn = FALSE)
             para = para[ !para %in% ""]

             if(length(para)==length(files)){
               message(paste0("generating video for: ", x))

               ari::ari_spin(paragraphs = para,
                        images = files,
                        voice = "Joanna",
                        output = file.path(vid_path,paste0(x,'.mp4')),
                        ffmpeg_opts = '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"',
                        verbose = TRUE,
                        audio_codec = "libfdk_aac"
               )
               ## update df
               df$vid_file[df$lesson==x] <- file.path(vid_path,paste0(x,'.mp4'))
               df$has_vid_file[df$lesson==x] <- TRUE
               df$mod_time_vid[df$lesson==x] <- ymd_hms(file.info(file.path(
                 vid_path,paste0(x,'.mp4')))$mtime)
             }else{
               message(paste0("attempted but failed to generate video for: ", x))
               warning(paste0(x, " has ", length(para),
                              " paragraphs of text and ",
                              length(files)," images" ))
             }
           }
         })
}
