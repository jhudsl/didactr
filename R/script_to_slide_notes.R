#' Upload script to Google Slide Deck Notes
#'
#' @param course_dir directory with course materials
#' @param ... arguments to pass to \code{\link{check_course}}
#'
#' @return A \code{data.frame} the same format as
#' a course summary
script_to_slide_notes = function(course_dir = ".",
                                 ...) {
  df = check_course(course_dir = course_dir,
                    ...)
  df = df$course_summary
  i = 1
  df$script_uploaded = FALSE
  for (i in seq(nrow(df))) {
    scr = readLines(df$scr_file[i], warn = FALSE)
    scr = scr[scr != ""]

    id = df$id[i]
    notes = notes_from_slide(id = id)

    if (length(scr) == length(notes)) {
      res = gs_replace_notes(id, notes = scr)
      df$script_uploaded[i] = TRUE
    }
  }
  return(df)
}
