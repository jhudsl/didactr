#' Translate Course to Another Language
#'
#' @param course_dir directory with course materials
#' @param detect Should \code{\link{gl_detect_file}} be run to
#' check that the language is not currently the target?
#' Must be google Language authorized using \code{gl_auth}
#' @param target language to translate to, see \code{gl_translate}
#' @param verbose Print diagnostic messages
#' @param overwrite Should files be overwritten?
#' If not, they are put in a temporary directory and translated.
#' @param ... additional arguments to pass to
#' \code{\link{translate_manuscript}} and
#' \code{\link{translate_script}}
#' @param sleep_time time in seconds to sleep before running
#' next call
#' @param use_book Use \code{Book.txt} to keep only the
#' manuscript files that were specified, passed to
#' \code{\link{check_course}}
#' @param translate_slides Should slides be translated as well?
#' If so, \code{\link{copy_and_translate_slide}} will be done
#' with the Google slide ID and replaced throughout the manuscript.
#' @param trash_same_gs_name passed to
#' \code{\link{copy_and_translate_slide}}
#' Should other Google slide decks
#' with the same name be trashed before copying?  If not,
#' can fill up your drive.
#'
#' @return A result from \code{\link{check_course}}
#' @export
#'
#' @importFrom rgoogleslides add_delete_text_request add_insert_text_request
#' @importFrom googledrive is_dribble
#'
#' @examples
#' \dontrun{
#' course_dir = "~/Dropbox/Projects/CDS/cds_dataanalysis"
#'
#' library(googledrive)
#' check_didactr_auth()
#'
#' res = translate_course(course_dir)
#'
#' }
translate_course = function(
  course_dir,
  target = "es",
  translate_slides = FALSE,
  detect = TRUE,
  verbose = TRUE,
  use_book = TRUE,
  overwrite = FALSE,
  sleep_time = 0,
  trash_same_gs_name = FALSE,
  ...) {

  if (!is_language_auth()) {
    stop("Google Language is not Authorized, see gl_auth")
  }

  # If not, copy all
  if (!overwrite) {
    course_dir = normalizePath(course_dir)
    tdir = tempfile()
    dir.create(tdir, recursive = TRUE)
    if (verbose) {
      message("Copying files to ", tdir)
    }
    file.copy(
      from = course_dir,
      to = tdir,
      recursive = TRUE)
    tdir = file.path(tdir, basename(course_dir))
    course_dir = tdir
  }

  res = check_course(
    course_dir,
    require_authorization = FALSE,
    save_metrics = FALSE,
    use_book = use_book)
  cdf = res$course_summary
  # if (use_book) {
  #   book_txt = readLines(res$paths$book_txt)
  #   book_txt = trimws(book_txt)
  #   cdf = cdf[ basename(cdf$md_file) %in% book_txt,]
  # }

  file = cdf$md_file[1]
  results = sapply(cdf$md_file, function(file) {
    if (verbose) {
      message(file)
    }
    if (file.exists(file)) {
      Sys.sleep(sleep_time)
      detection_true = TRUE
      if (detect) {
        det = didactr::gl_detect_file(file)
        detection_true = det$language != target
      }
      if (detection_true) {
        x = translate_manuscript(
          file, target = target,
          verbose = verbose,
          ...)
        out_txt = x$translatedText
        writeLines(out_txt, file)
      }
    }
    return(file)
  })


  file =  cdf$scr_file[1]
  results = sapply(cdf$scr_file, function(file) {
    if (verbose) {
      message(file)
    }
    if (file.exists(file)) {
      Sys.sleep(sleep_time)
      detection_true = TRUE
      if (detect) {
        det = didactr::gl_detect_file(file)
        detection_true = det$language != target
      }
      if (detection_true) {
        x = translate_script(
          file, target = target,
          verbose = verbose,
          ...)
        out_txt = x$translatedText
        writeLines(out_txt, file)
      }
    }
    return(file)
  })


  results = sapply(cdf$md_file, function(file) {
    if (verbose) {
      message(paste0("Slide deck for ", file))
    }
    if (file.exists(file)) {
      Sys.sleep(sleep_time)
      if (translate_slides) {
        gs_id = gs_id_from_slide(file)
        stopifnot(length(gs_id) == 1)
        if (!is.na(gs_id)) {
          gs_result = copy_and_translate_slide(
            gs_id,
            target = target,
            detect = detect,
            verbose = verbose,
            trash_same_gs_name = trash_same_gs_name)
          new_id = gs_result$id
          out_txt = readLines(file)
          out_txt = gsub(gs_id, new_id, out_txt)
          writeLines(out_txt, file)
        }
      }
    }
    return(file)
  })

  res = check_course(
    course_dir,
    require_authorization = FALSE,
    save_metrics = FALSE,
    use_book = use_book)

  return(res)
}
