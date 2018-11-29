#' Create video from script and PNGs
#'
#' @param course_status output from \code{\link{check_course}}
#' @param voice Voice to pass to \code{\link{ari_spin}}
#' @param audio_codec audio codec to pass to \code{\link{ari_spin}}
#' @param verbose Should diagnostic messages be printed
#' @param ffmpeg_opts ffmpeg options to pass to \code{\link{ari_spin}}
#' @param divisible_height should heights be divisible?
#' Advanced usage to pass to \code{\link{ari_spin}}
#' @param video_bitrate video bitrate to enforce.
#' Advanced usage to pass to \code{\link{ari_spin}}
#' @param ... additional arguments passed to \code{\link{ari_spin}}
#'
#' @return A `data.frame` output from \code{\link{check_course}}
#' @importFrom ari ari_spin
#' @export
#'
create_videos <- function(
  course_status = NULL,
  voice = "Joanna",
  ffmpeg_opts = '-minrate 16M -vf "scale=1200:720"',
  divisible_height = FALSE,
  video_bitrate = "16M",
  audio_codec = NULL,
  verbose = TRUE,
  ...){

  if (is.character(course_status)) {
    course_status = check_course(course_dir = course_status)
  }
  df = course_status$course_summary
  paths = course_status$paths

  if (!"has_vid_file" %in% colnames(df)) {
    warning("Creating videos, but ")
  }
  ## create video: ari_spin()
  ## if no video link in df
  ## OR if pngs are more recent than youtube video
  sapply(df$lesson,
         function(x) {
           lesson_index = df$lesson == x
           idf = df[lesson_index,]
           if (!idf$has_vid_file | idf$vid_more_recent | idf$scr_more_recent) {
             files <- list.files(
               path = file.path(paths$img_path, x),
               full.names = TRUE,
               pattern = "[.]png")
             # do all have the dash, then digit png?
             check = all(grepl(".*-\\d*[.]png", files))
             run_old_way = FALSE
             if (!check) {
               slide_df = gs_slide_df(idf$id)
               slide_df$png = file.path(
                 file.path(paths$img_path, idf$lesson),
                 paste0(slide_df$objectId, ".png"))
               if (!all(file.exists(slide_df$png))) {
                 run_old_way = TRUE
               } else {
                 files = slide_df$png
               }
             } else {
               string = ".*-(\\d*)[.]png"
               index = sub(string, "\\1", basename(files))
               index = as.numeric(index)
               if (any(is.na(index))) {
                 run_old_way = TRUE
               } else {
                 files = files[order(index)]
               }
             }

             # ## reorder im age files if they end in -1 rather than -01
             if (run_old_way) {
               if (length(grep("[0][0-9].png",files[1]))<1){
                 files1 <- list.files(file.path(paths$img_path,x),
                                      pattern = "-[0123456789].png",
                                      full.names = TRUE)

                 files2 <- list.files(file.path(paths$img_path, x),
                                      pattern = "-[0123456789][0123456789].png",
                                      full.names = TRUE)

                 files <- c(files1, files2)
               }
             }

             para = readLines(
               file.path(paths$scr_path, paste0(x, '_script.md')),
               warn = FALSE)
             para = para[ !para %in% c("", " ")]

             if (length(para) == length(files)) {
               message(paste0("generating video for: ", x))
               if (is.null(audio_codec)) {
                 audio_codec = ari::get_audio_codec()
               }
               # ari::ari_spin(
               #   paragraphs = para,
               #   images = files,
               #   voice = voice,
               #   output = file.path(paths$vid_path,paste0(x,'.mp4')),
               #   ffmpeg_opts = '-vf "scale=trunc(iw/2)*2:trunc(ih/2)*2"',
               #   verbose = verbose,
               #   audio_codec = audio_codec,
               #   ...
               # )
               res = ari::ari_spin(
                 paragraphs = para,
                 images = files,
                 voice = voice,
                 output = file.path(paths$vid_path,paste0(x,'.mp4')),
                 divisible_height = divisible_height,
                 video_bitrate = video_bitrate,
                 ffmpeg_opts = ffmpeg_opts,
                 verbose = verbose,
                 audio_codec = audio_codec,
                 ...
               )

               ### Ask SHANNON - does this even matter since running
               ### check_course below?
               ## update df
               df$vid_file[lesson_index] <- file.path(paths$vid_path,paste0(x,'.mp4'))
               df$has_vid_file[lesson_index] <- TRUE
               df$mod_time_vid[lesson_index] <- ymd_hms(file.info(file.path(
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
