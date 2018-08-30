#' Get Google Slide Elements IDs
#'
#' @param id Slide id passed to \code{\link{get_slides_properties}} after passing
#' through \code{\link{as_id}}
#'
#' @return A \code{data.frame} of the identifiers and properties of the
#' slides
#' @export
#' @importFrom rgoogleslides get_slides_properties
#' @examples \dontrun{
#' id = "1XoRj0pwaLI34XKZ7TljVeDHu-tbgGmXRmQa528JIwmw"
#' all_notes = notes_from_slide(id)
#' slides = gs_slide_df(id)
#' slides$slideProperties$notesPage$pageElements[[1]]
#' pe = slides$slideProperties$notesPage$pageElements
#' text = pe[[1]]$shape$text$textElements[[2]]$textRun$content
#' notes = sapply(pe,
#' function(r) {
#' res = sapply(r$shape$text$textElements, function(x) x$textRun$content)
#' res = unlist(res)
#' res = res[!is.na(res)]
#' if (is.null(res)) {
#' res = ""
#' }
#' paste(res, collapse = " ")
#' })
#' notes
#' notes2 = notes_from_slide(id)
#' }
gs_slide_df = function(id) {
  check_didactr_auth()
  if (inherits(id, "data.frame")) {
    id = id$id[1]
  }
  id = as.character(id)
  # if (TRUE){
    res = rgoogleslides::get_slides_properties(id = id)
  # } else {
    # res = gs_get_slides(id)
    # res = res$parsed
  # }
  slides = res$slides
  return(slides)
}

#' @export
#' @rdname gs_slide_df
notes_from_slide = function(id) {
  slides = gs_slide_df(id)
  pe = slides$slideProperties$notesPage$pageElements
  notes = sapply(
    pe,
    function(r) {
      res = sapply(r$shape$text$textElements, function(x) {
        x$textRun$content
      })
      res = unlist(res)
      res = res[!is.na(res)]
      if (is.null(res)) {
        res = ""
      }
      paste(res, collapse = " ")
    })
  notes
}

#' @export
#' @rdname gs_slide_df
#' @importFrom httr config GET accept_json content
#' @importFrom jsonlite fromJSON
gs_get_slides = function(id) {
  url = "https://slides.googleapis.com/v1/presentations/"
  url = paste0(url, id)
  token = didactr_token()
  config <- httr::config(token = token)
  call_result <- httr::GET(url, config = config, httr::accept_json())
  if (httr::status_code(call_result) != 200) {
    stop("ID provided does not point towards any slide")
  }
  run_content = httr::content(call_result, as = "text")
  result = jsonlite::fromJSON(run_content)
  res = list(response = call_result,
             content = run_content,
             parsed = result)
  return(res)
}
