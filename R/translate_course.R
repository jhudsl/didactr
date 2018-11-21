#' Translate Course to Another Language
#'
#' @param course_dir directory with course materials
#' @param detect Should \code{\link{gl_detect_file}} be run to
#' check that the language is not currently the target?
#' Must be google Language authorized using \code{\link{gl_auth}}
#' @param target language to translate to, see \code{\link{gl_translate}}
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
  detect = TRUE,
  verbose = TRUE,
  use_book = TRUE,
  overwrite = FALSE,
  sleep_time = 0,
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
      det = didactr::gl_detect_file(file)
      if (detect & det$language != target) {
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
      det = didactr::gl_detect_file(file)
      if (detect & det$language != target) {
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

  res = check_course(
    course_dir,
    require_authorization = FALSE,
    save_metrics = FALSE,
    use_book = use_book)

  return(res)
}
