#' Create video from script and PNGs
#'
#' @param course_status output from \code{\link{check_course}}
#' @param voice Voice to pass to \code{\link{ari_spin}}
#' @param service speech synthesis service to use, passed to \code{\link{tts}},
#' Either "amazon", "microsoft", or "google".
#' @param verbose print diagnostic messages
#' @param ... additional arguments passed to \code{\link{ari_spin}}
#'
#' @return A `data.frame` output from \code{\link{check_course}}
#' @importFrom ari ari_spin
#' @export
#'
create_videos <- function(
  course_status = ".",
  voice = text2speech::tts_default_voice(service = service),
  service = "google",
  verbose = TRUE,
  ...){

  exec = try({
    ari::ffmpeg_exec()
  })
  msg = "ffmpeg not detected, run didactr::install_ffmpeg()"
  if (inherits(exec, "try-error")) {
    stop(msg)
  }
  if (!file.exists(exec)) {
    warning(msg)
  }
  if (is.character(course_status)) {
    course_status = check_course(course_dir = course_status,
                                 require_authorization = FALSE)
  }
  df = course_status$course_summary
  paths = course_status$paths

  if (!"has_vid_file" %in% colnames(df)) {
    warning("Creating videos, but no indicator of having video files")
  }
  ## create video: ari_spin()
  ## if no video link in df
  ## OR if pngs are more recent than youtube video
  sapply(df$lesson,
         function(x) {
           lesson_index = df$lesson == x

           idf = df[lesson_index,]

           if (
             na_true(
               (!idf$has_vid_file | idf$vid_more_recent | idf$scr_more_recent)
             )
           ){
             files <- list.files(
               path = file.path(paths$img_path, x),
               full.names = TRUE,
               pattern = "[.]png")
             # do all have the dash, then digit png?
             check = all(grepl(".*-\\d*[.]png", files)) &
               length(files) > 0
             check = check & !is.na(idf$id)
             run_old_way = FALSE
             if (!check) {
               slide_df = gs_slide_df(idf$id)
               slide_df$png = file.path(
                 file.path(paths$img_path, idf$lesson),
                 paste0(slide_df$objectId, ".png"))
               if (!all(file.exists(slide_df$png))) {
                 gs_res = gs_convert(
                   id = idf$id,
                   PPTX = FALSE,
                   use_gs_ids = TRUE)
                 slide_df$png = gs_res$images
               }
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
             # allows us to use ; for no words
             semi_colon = trimws(para) == ";"
             if (any(semi_colon)) {
               # need space bc ari_spin removes ""
               para[semi_colon] = " "
             }

             if (length(para) == length(files)) {
               if (verbose) {
                 message(paste0("generating video for: ", x))
               }

               res = ari::ari_spin(
                 paragraphs = para,
                 images = files,
                 voice = voice,
                 output = file.path(paths$vid_path,paste0(x,'.mp4')),
                 service = service,
                 verbose = verbose,
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
                     save_metrics = course_status$save_metrics,
                     require_authorization = FALSE)
  return(ret)
}
