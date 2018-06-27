
#' Create PDF and PNGs from Google Slides
#'
#' @param course_status output from \code{\link{check_course}}
#' @param ... additional arguments passed to \code{\link{gs_convert}}
#'
#' @return Downloaded PDF and PNGs.
#' @export
create_images <- function(course_status = NULL, ...) {
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
  sapply(df$id,
         function(x) {
           if(is.na(x)){}else if((is.na(df$pdf[which(df$id==x)]) & !is.na(x))|(df$gs_more_recent[which(df$id==x)])){
             message(paste0("Converting PDFs for: ", df$lesson[which(df$id==x)]))
             out_dir = file.path(paths$img_path, df$lesson[which(df$id==x)])
             res = gs_convert(id = x, PPTX = FALSE,
                              out_dir = out_dir,
                              output_type = "png", ...)
             filename =  paste0(df$course_info[which(df$id==x)],".pdf")
             file.copy(res$pdf, to=file.path(paths$img_path,df$lesson[which(df$id==x)],filename),
                       overwrite=TRUE)
           }})
  ret = check_course(course_dir = course_status$course_dir,
                     save_metrics = course_status$save_metrics)

  return(ret)
}
