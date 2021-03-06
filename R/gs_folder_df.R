

#' Get Google Folder Contents IDs
#'
#' @param id Folder id passed to \code{\link{drive_ls}} after passing
#' through \code{\link{as_id}}
#' @param slides_only Return only elements that are
#' presentations
#'
#' @return A \code{data.frame} of the identifiers and properties of the
#' objects
#' @export
#' @examples \dontrun{
#' folder_id = "1GCBE9EAgY3-i1bq9UYf5ec5s9X5TOwEK"
#' df = gs_folder_df(folder_id)
#' }
gs_folder_df = function(id, slides_only = FALSE) {
  name = mime_type = NULL
  rm(list = c("mime_type", "name"))
  check_didactr_auth()
  if (inherits(id, "data.frame")) {
    id = id$id[1]
  }
  id = as.character(id)
  id = ariExtra::get_folder_id(id)
  res = googledrive::drive_ls(path = as_id(id), recursive = TRUE)
  shared_res = googledrive::drive_ls(
    path = as_id(id),
    recursive = TRUE,
    q = "sharedWithMe")
  res = dplyr::bind_rows(res, shared_res)
  res = res %>%
    arrange(name)
  if (nrow(res) == 0) {
    warning("No folder was found!")
    return(NULL)
  }
  res$mime_type = vapply(res$drive_resource,
                         function(x) {
                           mt = x$mimeType
                           if (is.null(mt)) {
                             mt = NA_character_
                           }
                           return(mt)
                         }, FUN.VALUE = character(1L))
  if (slides_only) {
    res = res %>%
      filter(grepl("presentation", mime_type))
  }
  return(res)
}

