#' Create video from script and PNGs
#'
#' @param course_status output from \code{\link{check_course}}
#' @param voice Voice to pass to \code{\link{ari_spin}}
#' @param audio_codec audio codec to pass to \code{\link{ari_spin}}
#' @param verbose Should diagnostic messages be printed
#' @param ... additional arguments passed to \code{\link{ari_spin}}
#'
#' @return A `data.frame` output from \code{\link{check_course}}
#' @importFrom ari ari_spin
#' @export
#'

create_videos <- function(course_status = NULL,
                          voice = "Joanna",
                          audio_codec = "libfdk_aac",
                          verbose = TRUE,
                          ...){
  df = course_status$course_summary
  paths = course_status$paths

  ## create video: ari_spin()
  ## if no video link in df
  ## OR if pngs are more recent than youtube video
  sapply(df$lesson,
         function(x) {
           if(!df$has_vid_file[df$lesson==x]|df$vid_more_recent[df$lesson==x]|df$scr_more_recent[df$lesson==x] ){
             files <- grep("[.]png", list.files(file.path(paths$img_path, x), full.names = TRUE),
                           value = TRUE)

             ## reorder image files if they end in -1 rather than -01
             if(length(grep("[0][0-9].png",files[1]))<1){
               files1 <- grep("[.]png", list.files(file.path(paths$img_path,x),
                                                   pattern = "-[0123456789].png",
                                                   full.names = TRUE),
                              value = TRUE)
               files2 <- grep("[.]png", list.files(file.path(paths$img_path, x),
                                                   pattern = "-[0123456789][0123456789].png",
                                                   full.names = TRUE),
                              value = TRUE)
               files <- c(files1, files2)
             }

             para = readLines(file.path(paths$scr_path, paste0(x, '_script.md')), warn = FALSE)
             para = para[ !para %in% ""]

             if(length(para) == length(files)){
               message(paste0("generating video for: ", x))

               ari::ari_spin(paragraphs = para,
                        images = files,
                        voice = voice,
                        output = file.path(paths$vid_path,paste0(x,'.mp4')),
                        ffmpeg_opts = '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"',
                        verbose = verbose,
                        audio_codec = audio_codec,
                        ...
               )
               ## update df
               df$vid_file[df$lesson==x] <- file.path(paths$vid_path,paste0(x,'.mp4'))
               df$has_vid_file[df$lesson==x] <- TRUE
               df$mod_time_vid[df$lesson==x] <- ymd_hms(file.info(file.path(
                 paths$vid_path,paste0(x,'.mp4')))$mtime)
             }else{
               message(paste0("attempted but failed to generate video for: ", x))
               warning(paste0(x, " has ", length(para),
                              " paragraphs of text and ",
                              length(files)," images" ))
             }
           }
         })
  ret = check_course(course_dir = course_status$course_dir,
                     save_metrics = course_status$save_metrics)
  return(ret)
}
