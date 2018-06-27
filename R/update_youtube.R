#' Update RDS object that tracks YouTube uploads
#'
#' @param up output from \code{\link{vids_to_youtube}}
#' @param file object created in \code{\link{vids_to_youtube}}
#' @param lesson object created in \code{\link{vids_to_youtube}}
#' @param metric_path Path to the metrics file
#' @param timezone Timezone to be used?
#' @param save_metrics Should an `rds` file be saved of the `data.frame`?
#'
#' @return A \code{data.frame} of all uploaded youtube videos
#' @importFrom dplyr distinct
#' @export
update_youtube <- function(
  up = NULL,
  file = NULL,
  lesson = NULL,
  metric_path = NULL,
  save_metrics = TRUE,
  timezone = "America/New_York") {

  yt_md_link = lesson = time_published = NULL
  rm(list = c("yt_md_link", "lesson", "time_published"))

  if (is.null(metric_path)) {
    metric_path = up$metric_path
  }


  if (save_metrics) {
    if (is.null(metric_path)) {
      stop("Metrics are to be saved, but metric_path not specified!")
    }
    yt_file = file.path(metric_path, "youtube_uploads.rds")
    ## create file tracking object output
    if (!file.exists(yt_file)) {
      youtube_uploads <- NULL
    } else {
      youtube_uploads = readRDS(yt_file)
    }
  }

  # If someone doesn't specify lesson, grab it from the filename
  # from the request
  if (is.null(file)) {
    file = up$file
    if (is.null(file)) {
      uploaded_file = up$request$request$fields$y
      file = basename(uploaded_file$path)
    }
  }
  stopifnot(!is.null(file))

  # make the lesson from the filnemae if not specified
  if (is.null(lesson)) {
    lesson = up$lesson
    if (is.null(lesson)) {
      lesson = sub("[.]mp4$", "", basename(file))
    }
  }
  stopifnot(!is.null(lesson))

  if (!is.null(up$content)) {
    yt_df <- as_data_frame(t(unlist(up$content))) %>%
      mutate(file = basename(file),
             lesson = lesson,
             url = up$url,
             time_published = lubridate::ymd_hms(up$content$snippet$publishedAt),
             time_published = lubridate::with_tz(time_published,
                                                 tzone = timezone)
      ) %>%
      select(file, lesson, url, time_published, everything()) %>%
      rename(yt_id = id)
  } else {
    yt_df = NULL
  }

  youtube_uploads = bind_rows(youtube_uploads, yt_df)
  youtube_uploads = distinct(youtube_uploads)
  if (save_metrics) {
    saveRDS(youtube_uploads, file = yt_file, compress = "xz")
  }
  return(youtube_uploads)
}
