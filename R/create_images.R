
#' Create PDF and PNGs from Google Slides
#'
#' @param course_status output from \code{\link{check_course}}
#' @param use_gs_ids use Google slide identifiers for the naming,
#' passed to \code{\link{gs_convert}}
#' @param ... additional arguments passed to \code{\link{gs_convert}}
#'
#' @return Downloaded PDF and PNGs.
#' @export
create_images <- function(
  course_status = NULL,
  use_gs_ids = FALSE,
  ...) {
  if (is.character(course_status)) {
    course_status = check_course(course_dir = course_status)
  }
  df = course_status$course_summary
  paths = course_status$paths

  ## convert PDF to PNGs and downlaod PDF
  ## gs_convert() and file.copy()
  ## do this if:
  ## PDF not yet downloaded, but google slides exist
  ## OR mod_time_gs > mod_time_pngs
  # keep_df = df[ (is.na(df$pdf) & !is.na(df$id)) | (df$gs_more_recent), ]
  sapply(df$id,
         function(x) {
           if (!is.na(x)){
             index = which(df$id == x)
             idf = df[index, ]
             check = (is.na(idf$pdf) & !is.na(x)) | (idf$gs_more_recent)
             if (check) {
               message(paste0("Converting PDFs for: ", idf$lesson))
               out_dir = file.path(paths$img_path, idf$lesson)
               res = gs_convert(
                 id = x,
                 PPTX = FALSE,
                 out_dir = out_dir,
                 output_type = "png",
                 use_gs_ids = use_gs_ids,
                 ...)
               filename =  paste0(idf$course_info,".pdf")
               file.copy(res$pdf,
                         to = file.path(paths$img_path, idf$lesson,filename),
                         overwrite = TRUE)
             }
           }
         })
  ret = check_course(course_dir = course_status$course_dir,
                     save_metrics = course_status$save_metrics)

  return(ret)
}
