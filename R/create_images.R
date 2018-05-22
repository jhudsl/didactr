
#' Create PDF and PNGs from Google Slides
#'
#' @param course_status output from \code{\link{check_course}}
#'
#' @return Downloaded PDF and PNGs.
#' @export

create_images <- function(course_status = NULL){
  df = course_status$course_summary
  ## convert PDF to PNGs and downlaod PDF
  ## gs_convert() and file.copy()
  ## do this if:
  ## PDF not yet downloaded, but google slides exist
  ## OR mod_time_gs > mod_time_pngs
  sapply(df$id,
         function(x) {
           if((is.na(df$pdf[df$id==x]) & !is.na(x))|(df$gs_more_recent[df$id==x])){
             message(paste0("Converting PDFs for: ", df$lesson[df$id==x]))
             out_dir = file.path(img_path, df$lesson[df$id==x])
             res = gs_convert(id = x, PPTX = FALSE,
                              out_dir = out_dir,
                              output_type = "png")
             filename =  paste0(df$cnum[df$id==x] ,"_", df$course[df$id==x],
                                "_", df$lesson_name[df$id==x],".pdf")
             file.copy(res$pdf, to=file.path(img_path,df$lesson[df$id==x],filename),
                       overwrite=TRUE)
             message(paste0("Reminder to go re-run df images code"))
           }})
}
