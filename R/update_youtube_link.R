#' Update Youtube Link
#'
#' @param course_status output from \code{\link{check_course}}
#' @param youtube_uploads The `data.frame` output from
#' \code{\link{update_youtube}}.  Will look in the metric path if not
#' specified.
#'
#' @return An output after running \code{\link{check_course}}.
#' @export
update_youtube_link <- function(course_status = NULL,
                                youtube_uploads = NULL) {
  if (is.character(course_status)) {
    course_status = check_course(course_dir = course_status)
  }
  df = course_status$course_summary
  paths = course_status$paths

  if (is.null(youtube_uploads)) {
    yt_file = file.path(paths$met_path, "youtube_uploads.rds")
    if (file.exists(yt_file)) {
      youtube_uploads = readRDS(yt_file)
    } else {
      if (is.null(youtube_uploads)) {
        stop("YouTube uploads not found")
      }
    }
  }

  if (!("lesson" %in% colnames(youtube_uploads))) {
    youtube_uploads = youtube_uploads %>%
      mutate(lesson = sub("[.]mp4$", "", basename(file)))
  }
  df = left_join(df, youtube_uploads, by = "lesson")

  if (any(is.na(df$time_published))) {
    warning(paste0("Some times are NA for published YT videos, ",
                   "may not update correctly!"))
  }
  # keep most recent published youtube video
  df = df %>%
    group_by(lesson) %>%
    arrange(desc(time_published)) %>%
    dplyr::slice(1)

  # do we need to update any of these?
  keep = df %>%
    filter(is.na(yt_md_link) | (url != yt_md_link & !is.na(url)))

  if (nrow(keep) > 0) {

    # replacing the actual links
    md_files = keep$md_file
    new_urls = keep$url
    updated_urls = mapply(function(fname, new_url) {

      message(paste0("Updating youtube link in manuscript file: ", fname))
      txt  <- readLines(fname)
      # identify which link to edit for the video
      line <- grep(pattern = png_pattern(), txt, perl = TRUE)

      # replace empty () or (http://etc) with new link
      txt[line] <- gsub("\\(.+\\)|\\(\\)",
             paste0("(", new_url, ")"), txt[line])

      ###########
      # rewrite the md file
      ###########
      writeLines(txt, con = fname)
      return(TRUE)
    }, md_files, new_urls)
    rm(updated_urls)

    ###########
    # double check things went well - no gsub errors
    ###########
    check_urls = mapply(function(fname, new_url) {
      txt  <- readLines(fname)
      # identify which link to edit for the video
      line <- grep(pattern = png_pattern(), txt, perl = TRUE)
      urls <- gsub(".*]\\((.*)\\)","\\1", txt[line])
      urls = unique(urls)
      all(urls %in% new_url)
    }, md_files, new_urls)
    if (any(!check_urls)) {
      bad = names(check_urls)[!check_urls]
      stop(paste0("URL replacement for ", paste(bad, collapse = ", "),
                     " did not work well"))
    }
  }

  ret = check_course(course_dir = course_status$course_dir,
                     save_metrics = course_status$save_metrics)
  return(ret)
}
