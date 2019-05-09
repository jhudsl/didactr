#' Translate Google Slide Text to Another Language
#'
#' @param id Google Slide ID, usually from \code{\link{drive_ls}}
#' @param detect Should \code{\link{gl_detect_file}} be run to
#' check that the language is not currently the target?
#' Must be google Language authorized using \code{\link{gl_auth}}
#' @param target language to translate to, see \code{\link{gl_translate}}
#' @param verbose Print diagnostic messages
#'
#' @return A list of results from \code{\link{commit_to_slides}}
#' @export
#'
#' @importFrom rgoogleslides add_delete_text_request add_insert_text_request
#' @importFrom googledrive is_dribble
#'
#' @examples
#' \dontrun{
#' library(googledrive)
#' check_didactr_auth()
#' # id = "147UQaZBB5RoTpzsNiqkqEr4N8fcHBMB6eNr_5IdksRk"
#' info = drive_find("datatables", n_max = 25, type = "presentation")
#' info = info[grepl("^06" ,info$name), ]
#' if (nrow(info) > 0) {
#' info = info[1,]
#' xid = drive_cp(info, name = paste0("Translated ",
#' info$name))
#' id = xid
#' translated = translate_slide(id, target = "es")
#' drive_trash(id)
#' }
#'
#' }
translate_slide = function(
  id,
  target = "es",
  detect = TRUE,
  verbose = TRUE,
  ...) {

  check_didactr_auth(...)

  if (!is_language_auth()) {
    stop("Google Language is not Authorized, see gl_auth")
  }
  text_content = NULL
  rm(list = "text_content")

  if (verbose) {
    message("Pulling slide from Google Slides")
  }
  if (is.character(id)) {
    id = get_slide_id(id)
    stopifnot(length(id) == 1)
  }
  if (googledrive::is_dribble(id)) {
    id = id$id
  }
  sp = rgoogleslides::get_slides_properties(id)
  pages = sp$slides$objectId

  page_id = pages[2]
  n_pages = length(pages)
  if (verbose) {
    message("Found ", n_pages, " pages")
  }

  if (n_pages == 0) {
    warning(paste0("No pages found for slide id:", id))
    return(NULL)
  }
  result_list = vector(
    mode = "list",
    length = n_pages)
  names(result_list) = pages

  # Get all slide text
  tb_data = result_list
  for (page_id in pages) {

    if (verbose) {
      message("Pulling ", page_id, " page_id")
    }
    pp = rgoogleslides::get_slide_page_properties(
      id,
      page_object_id = page_id)
    tb_df = pp$get_text_boxes()
    tb_df = tb_df %>%
      dplyr::filter(text_content != "") %>%
      dplyr::mutate(text_content = sub("\n$", "", text_content))
    tb_data[[page_id]] = tb_df
  }
  tb_df = dplyr::bind_rows(tb_data, .id = "page_id")

  make_bad_string = function() {
    x = round(runif(1, min = 1e5, max = 1000000))
    x = paste0(x, ";")
  }


  # Actually translate
  L = list(id = id,
           table_of_changes = tb_df)
  if (nrow(tb_df) > 0) {
    bad_string =  make_bad_string()
    for (i in 1:10) {
      # just make another
      if (any(grepl(bad_string, tb_df$text_content))) {
        bad_string =  make_bad_string()
      }
    }
    stopifnot(!any(grepl(bad_string, tb_df$text_content)))
    tb_df$text_content = gsub("\n", bad_string,
                              tb_df$text_content)

    tb = tb_df$text_content
    file = tempfile()
    writeLines(tb, con = file)
    if (verbose) {
      message("Temporary File Created: ", file,
              " with bad_string: ", bad_string)
    }
    if (detect) {
      if (verbose) {
        message("Detecting Language")
      }
      out = gl_detect_file(file)
      if (out$language == target) {
        message(page_id, " already in target language")
        return(NULL);
      }
    }

    df = chunk_google_translate(
      file,
      target = target,
      chunk = TRUE,
      fix_header = FALSE)
    df$translatedText = gsub(
      bad_string, "\n",
      df$translatedText)
    df$text = gsub(
      bad_string, "\n",
      df$text)
    tb_df$text_content = gsub(
      bad_string, "\n", tb_df$text_content)
    tb = gsub(bad_string, "\n", tb)

    tb_new = df$translatedText
    stopifnot(length(tb) == length(tb_new))
    stopifnot(!any(grepl(bad_string, tb_new)))
    stopifnot(!any(grepl(bad_string, tb)))
    request = NULL
    tb_df$text_replacement = tb_new
    for (itb in seq_along(tb)) {
      # delete text
      request = add_delete_text_request(
        google_slides_request = request,
        object_id = tb_df$object_id[itb])
      # add text
      request = add_insert_text_request(
        google_slides_request = request,
        object_id = tb_df$object_id[itb],
        text = tb_df$text_replacement[itb]
      )
    }
    res = rgoogleslides::commit_to_slides(id, request)
    L$table_of_changes = tb_df
    L$request_result = res
  }
  return(L)
}

#' @export
#' @rdname translate_slide
#' @param gs_name Name of new Google slide deck
#' @param trash_same_gs_name Should other Google slide decks
#' with the same name be trashed before copying?  If not,
#' can fill up your drive.
#' @param ... arguments passed to
#' \code{\link{check_didactr_auth}}
#' @note Copies are put in the \code{didactr_translations}
#' folder in your Google Drive.
copy_and_translate_slide = function(
  id,
  gs_name = NULL,
  trash_same_gs_name = FALSE,
  target = "es",
  detect = TRUE,
  verbose = TRUE,
  ...) {
  check_didactr_auth(...)
  if (!is_language_auth()) {
    stop("Google Language is not Authorized, see gl_auth")
  }
  if (is.character(id)) {
    id = get_slide_id(id)
    stopifnot(length(id) == 1)
  }
  if (googledrive::is_dribble(id)) {
    id = id$id
  }
  info = drive_get(id = googledrive::as_id(id))
  stopifnot(nrow(info) == 1)
  if (is.null(gs_name)) {
    gs_name = paste0(target, "_Translated_", info$name)
  }
  folder_name = "didactr_translations"
  trans_fol = googledrive::drive_find(
    pattern = folder_name,
    type = "folder", n_max = 100,
    verbose = verbose)
  if (nrow(trans_fol) == 0) {
    trans_fol = googledrive::drive_mkdir(
      "didactr_translations",
      verbose = verbose)
  }

  if (trash_same_gs_name) {
    info = googledrive::drive_ls(
      path = trans_fol,
      gs_name,
      type = "presentation"
    )
    googledrive::drive_trash(info, verbose = verbose)
  }
  xid = googledrive::drive_cp(
    info, name = gs_name,
    path = trans_fol)
  Sys.sleep(3)
  translated = translate_slide(xid,
                               target = target,
                               detect = detect,
                               verbose = verbose,
                               ...)
  return(translated)
}
