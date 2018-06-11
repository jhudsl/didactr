#' Update RDS object that tracks YouTube uploads
#'
#' @param up output from \code{\link{vids_to_youtube}}
#' @param file object created in \code{\link{vids_to_youtube}}
#' @param lesson object created in \code{\link{vids_to_youtube}}
#' @param metric_path Path to the metrics file
#'
#' @return A \code{data.frame} of all uploaded youtube videos
#' @importFrom dplyr distinct
#' @export

update_youtube <- function(
  up, file=NULL, lesson=NULL,
  metric_path = NULL, timezone="America/New_York") {
  met_file = file.path(metric_path, "youtube_uploads.rds")
  ## create file tracking object output
  if (!file.exists(met_file)) {
    youtube_uploads <- NULL
  } else{
    youtube_uploads = readRDS(met_file)
  }

  yt_df <- as_data_frame(t(unlist(up$content))) %>%
    mutate(file = basename(file),
           lesson = lesson,
           url = up$url,
           time_published = ymd_hms(up$content$snippet$publishedAt),
           time_published = lubridate::with_tz(time_published, tz = timezone)
    ) %>%
    select(file, lesson, url, time_published, everything())

  youtube_uploads = bind_rows(youtube_uploads, yt_df)
  youtube_uploads = distinct(youtube_uploads)
  saveRDS(youtube_uploads, file = met_file, compress = "xz")
  return(youtube_uploads)
}
