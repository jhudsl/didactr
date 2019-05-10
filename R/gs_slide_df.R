#' Get Google Slide Elements IDs
#'
#' @param id Slide id passed to \code{\link{get_slides_properties}} after passing
#' through \code{\link{as_id}}
#' @param extract_code If you have text with \code{#rstats} in the
#' slide or \code{#rstats} in the Alt-text title, code will be
#' included
#'
#' @return A \code{data.frame} of the identifiers and properties of the
#' slides
#' @export
#' @importFrom rgoogleslides get_slides_properties
#' @examples \dontrun{
#' id = "1XoRj0pwaLI34XKZ7TljVeDHu-tbgGmXRmQa528JIwmw"
#' slides = gs_slide_df(id)
#' all_notes = notes_from_slide(id)
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
#' }
gs_slide_df = function(id, extract_code = TRUE) {
  check_didactr_auth()
  if (inherits(id, "data.frame")) {
    id = id$id[1]
  }
  id = as.character(id)
  id = didactr::get_slide_id(id)
  # if (TRUE){
  res = rgoogleslides::get_slides_properties(id = id)
  # } else {
  # res = gs_get_slides(id)
  # res = res$parsed
  # }
  slides = res$slides
  slides$png_url = paste0(
    "https://docs.google.com/presentation/d/",
    id, "/export/png?id=", id,
    "&pageid=", slides$objectId)
  slides$png_markdown = paste0("{format: png}\n![](", slides$png_url, ")\n")
  slides$png_df = dplyr::tibble(
    page_id = slides$objectId,
    png_url = slides$png_url,
    png_markdown = slides$png_markdown
  )
  slides = dplyr::as_tibble(slides)
  if (extract_code) {
    slides$code = gs_code_from_slides(slides)
  }
  return(slides)
}

#' @export
#' @rdname gs_slide_df
notes_from_slide = function(id) {
  slides = gs_slide_df(id)
  notes_from_slide_output(slides)
}

#' @export
#' @rdname gs_slide_df
gs_notes_from_slide = notes_from_slide

#' @export
#' @rdname gs_slide_df
#' @param slides an output of \code{gs_slide_df(id)}
notes_from_slide_output = function(slides) {
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
  notes = sub("\n$", "", notes)
  notes
}

#' @export
#' @rdname gs_slide_df
#' @importFrom httr config GET accept_json content
#' @importFrom jsonlite fromJSON
#' @examples \dontrun{
#' id = "1XoRj0pwaLI34XKZ7TljVeDHu-tbgGmXRmQa528JIwmw"
#' slides = gs_get_slides(id)
#' tfile = tempfile(fileext = ".txt")
#' writeLines(slides$content, con = tfile)
#' ss = strsplit(slides$content, split = "\n")[[1]]
#' ind = grep("uthinkk", ss)
#' ss[(ind -30):(ind + 5)]
#'
#' slides$parsed$slides$slideProperties$notesPage
#' slides$parsed$slides$slideProperties$notesPage$pageElements
#'
#'
#' }
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

#' @export
#' @rdname gs_slide_df
#' @examples \dontrun{
#' id = "1XoRj0pwaLI34XKZ7TljVeDHu-tbgGmXRmQa528JIwmw"
#' parsed = gs_get_slides(id)$parsed
#' parsed$slides$slideProperties$notesPage$notesProperties$speakerNotesObjectId
#'
#' notes_id = gs_speaker_notes_id(id)
#'
#' }
gs_speaker_notes_id = function(id) {
  slides = gs_get_slides(id)
  slides$parsed$slides$slideProperties$notesPage$notesProperties$speakerNotesObjectId
}


gs_replace_notes.deprecated = function(
  id, notes,
  force = FALSE
) {

  notes = as.character(notes)
  url = "https://slides.googleapis.com/v1/presentations/"
  url = paste0(url, id, ":batchUpdate")
  token = didactr_token()
  config <- httr::config(token = token)

  shape_ids = gs_speaker_notes_id(id)
  shape_ids = unname(shape_ids)

  curr_notes = notes_from_slide(id)
  notes = unname(notes)
  curr_notes = unname(curr_notes)

  stopifnot(length(shape_ids) == length(notes))
  make_delete = function(shape_id) {
    list(
      deleteText = list(
        objectId =  shape_id,
        textRange = list(type = 'ALL')
      )
    )
  }
  make_insert = function(shape_id, note) {
    list(
      insertText = list(
        objectId = shape_id,
        insertionIndex = 0,
        text = note
      )
    )
  }


  all_results = vector(
    mode = "list",
    length = length(notes))
  inote = 1
  for (inote in seq_along(notes)) {
    shape_id = shape_ids[[inote]]
    note = notes[[inote]]
    curr_note = curr_notes[[inote]]
    if (identical(curr_note, note) || force) {
      requests = list(
        make_delete(shape_id),
        make_insert(shape_id, note)
      )
      # don't delete if nothing there

      if (curr_note == "") {
        requests = list(
          make_insert(shape_id, note)
        )
      }

      out_json = jsonlite::toJSON(
        list(requests = requests),
        auto_unbox = TRUE)
      tfile = tempfile(fileext = ".json")
      writeLines(out_json, tfile)
      reqs = httr::upload_file(tfile)

      call_result <- httr::POST(
        url, config = config,
        body = reqs,
        httr::content_type_json())
      httr::warn_for_status(call_result)
      all_results[[inote]] = list(result = call_result,
                                  request = requests,
                                  request_file = tfile)
    }
  }
  return(all_results)

}


#' @export
#' @rdname gs_slide_df
#' @param notes a character vector of notes to be added to
#' the slides
#' @examples \dontrun{
#'
#' id = "1XoRj0pwaLI34XKZ7TljVeDHu-tbgGmXRmQa528JIwmw"
#' shape_ids = gs_speaker_notes_id(id)
#' notes = sample(letters, size = length(shape_ids))
#' res = gs_replace_notes(id, notes)
#' curr_notes = notes_from_slide(id)
#' all(curr_notes == notes)
#' }
gs_replace_notes = function(
  id, notes
) {

  check_didactr_auth()

  shape_ids = gs_speaker_notes_id(id)
  shape_ids = unname(shape_ids)

  notes = as.character(notes)
  notes = unname(notes)

  curr_notes = notes_from_slide(id)
  curr_notes = unname(curr_notes)

  stopifnot(length(shape_ids) == length(notes))

  all_results = vector(
    mode = "list",
    length = length(notes))
  inote = 1
  for (inote in seq_along(notes)) {
    gs_result = rgoogleslides::add_delete_text_request(
      object_id = shape_ids[inote]
    )
    # don't delete if nothing there
    curr_note = curr_notes[[inote]]
    if (curr_note == "") {
      gs_result = NULL
    }
    gs_result = rgoogleslides::add_insert_text_request(
      google_slides_request = gs_result,
      object_id = shape_ids[inote],
      text = notes[inote])
    res = rgoogleslides::commit_to_slides(
      id = id,
      google_slide_request = gs_result)

    all_results[[inote]] = list(
      response = res,
      note = notes[inote],
      shape_id = shape_ids[inote])
  }
  return(all_results)

}

# gs_slide_dt = function(id) {
#   png_url = png = page_id = NULL
#   rm(list = c("png", "png_url", "page_id"))
#
#   slides = gs_slide_df(id)
#   df = slides$png_df
#   df = df %>%
#     mutate(
#       png = paste0('<img src="', png_url, '">')
#       # png = paste0("![Image](", png_url, ")")
#     ) %>%
#     select(page_id, png)
#   dt = DT::datatable(df, escape = FALSE)
# }
