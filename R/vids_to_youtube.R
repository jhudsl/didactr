#' Upload videos to YouTube
#'
#' @param course_status object output from \code{\link{check_course}}
#' @param course_title Course to be used in title of youtube video
#' @param json Link to json file with credentials
#' @param ... arguments to pass to \code{\link{didactr_auth}}
#'
#' @return A list from \code{\link{check_course}} with another field of
#' \code{youtube_uploads}.
#' @export
#' @importFrom stringr str_to_title str_replace str_replace_all
#' @importFrom dplyr data_frame
vids_to_youtube <- function(
  course_status,
  course_title = NULL,
  json = NULL,
  ...) {
  authorized = check_didactr_auth(...)

  time_published = mod_time_vid = lesson_name = NULL
  rm(list = c("time_published", "mod_time_vid", "lesson_name"))

  make_video = lesson = NULL
  rm(list = c("make_video", "lesson"))
  # not sure if this matters
  if (!is.null(json) && !authorized) {
    yt_auth(json = json)
  }

  if (is.character(course_status)) {
    course_status = check_course(course_dir = course_status)
  }
  df = course_status$course_summary
  paths = course_status$paths

  yt_file = file.path(paths$met_path, "youtube_uploads.rds")
  if (file.exists(yt_file)) {
    youtube_uploads <- readRDS(yt_file)
  } else {
    youtube_uploads = NULL
  }

  # make sure course_title is there
  if (is.null(course_title)) {
    course_title = youtube_uploads$course_title
  }
  if (is.null(course_title)) {
    stop("Course Title required for upload to youtube")
  } else {
    course_title = unique(course_title)
    course_title = course_title[ !course_title %in% "" ]
    if (length(course_title) != 1) {
      stop("Course Title must be only one value")
    }
  }
  df = df %>%
    mutate(course_title = course_title)

  ################################################
  # add in
  df = df %>%
    mutate(lesson_title = paste0(course_title,": ", lesson_name))


  ## if no video link in df
  ## or if video link in df not the most updated
  if (!is.null(youtube_uploads)) {
    youtube_uploads = youtube_uploads %>%
      mutate(lesson = sub("[.]mp4$", "", basename(file)))
    df = left_join(df, youtube_uploads, by = "lesson")

    # keep most recent
    df = df %>%
      group_by(lesson) %>%
      arrange(desc(time_published)) %>%
      dplyr::slice(1)
    df = df %>%
      mutate(make_video = time_published < mod_time_vid,
             make_video = ifelse(is.na(make_video), TRUE, make_video))
    df = df %>%
      filter(make_video)
  }

  # no videos need to be made
  if (nrow(df) > 0) {
    df = df %>%
      group_by(lesson)


    for (irow in seq(nrow(df))) {
      idf = df[irow, ]
      message(paste0("uploading video to youtube for: ", idf$lesson_name))
      up = tuber::upload_video(file = idf$vid_file,
                        snippet = list(title = idf$lesson_title),
                        status = list(privacyStatus = "unlisted",
                                      license = "creativeCommon"))
      up$file = idf$vid_file
      up$lesson = idf$lesson
      up$metric_path = paths$met_path
      #update uploaded videos data frame
      yt_up <- update_youtube(
        up,
        metric_path = paths$met_path,
        save_metrics = course_status$save_metrics)
      youtube_uploads = bind_rows(yt_up, youtube_uploads)
    }
  }
  youtube_uploads = distinct(youtube_uploads)
  youtube_uploads$course_title = course_title

  ret = check_course(course_dir = course_status$course_dir,
                     save_metrics = course_status$save_metrics)
  ret$youtube_uploads = youtube_uploads
  return(ret)
}
