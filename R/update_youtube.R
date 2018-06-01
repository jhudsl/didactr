#' Create PDF and PNGs from Google Slides
#'
#' @param up output from \code{\link{vids_to_youtube}}
#' @param file object created in \code{\link{vids_to_youtube}}
#' @param lesson object created in \code{\link{vids_to_youtube}}
#'
#' @return Downloaded PDF and PNGs.
#' @export

update_youtube <- function(up, file=NULL, lesson=NULL){
  x = up
  ## create file tracking object output
  if(!file.exists(file.path(met_path,"youtube_uploads.rda"))){
    youtube_uploads <- data_frame()
  }else{
    load(file.path(met_path,"youtube_uploads.rda"))
  }
  yt_df <- as_data_frame(t(unlist(x$content))) %>%
    mutate(file=basename(file)) %>%
    mutate(lesson=lesson) %>%
    mutate(url=x$url) %>%
    mutate(time_published = ymd_hms(x$content$snippet$publishedAt)) %>%
    select(file,lesson,url,time_published,everything())
  if(!file.exists(file.path(met_path,"youtube_uploads.rda"))){
    youtube_uploads <- yt_df
    save(youtube_uploads,file =
           file.path(met_path,"youtube_uploads.rda"))
  }else{
    if(!yt_df$id %in% youtube_uploads$id){
    youtube_uploads <- bind_rows(youtube_uploads,yt_df)
    save(youtube_uploads,file =
         file.path(met_path,"youtube_uploads.rda"))
  }}
  return(youtube_uploads)
}
