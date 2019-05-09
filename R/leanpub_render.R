#' Render Leanpub document
#'
#' @param md_file Path to markdown file
#' @param output_options arguments to send to \code{\link{render}}
#' @param ... additional arguments to send to \code{\link{render}}
#' other than \code{input}, `output_dir`, and `output_format`,
#' and `output_options`
#'
#' @return HTML output filename
#' @export
#' @importFrom rmarkdown render pandoc_version
#' @importFrom rstudioapi viewer isAvailable
#' @importFrom utils browseURL
leanpub_render = function(
  md_file,
  output_options = list(self_contained = TRUE),
  remove_tests = TRUE,
  ...) {
  if (rmarkdown::pandoc_version() <= package_version("1.20")) {
    stop(paste0(
      "You must have pandoc version >= 1.20.  If ",
      "you have pandoc installed system-wide, please upgrade",
      ". Otherwise, upgrade RStudio version."))
  }
  md_file = normalizePath(md_file, mustWork = TRUE)
  tfile = basename(tempfile(fileext = ".md"))
  run_dir = dirname(md_file)
  tfile = file.path(run_dir, tfile)
  md_file =  process_leanpub_md(
    md_file = md_file,
    remove_tests = remove_tests)

  stub = sub("[.]md$", "", tfile)
  file.copy(md_file, tfile)
  on.exit({
    file.remove(tfile)
    file.remove(paste0(stub, ".html"))
    unlink(paste0(stub, "_files"), recursive = TRUE)
  })

  res = tempfile(fileext = ".html")
  result = rmarkdown::render(
    input = tfile,
    output_dir = run_dir,
    output_format = "html_document",
    output_options = output_options,
    ...)
  file.copy(result, res)
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(res)
  } else {
    browseURL(res)
  }
  # Sys.sleep(time = 2)
  return(res)
}

process_leanpub_md = function(md_file, remove_tests = TRUE) {
  if (remove_tests) {
    md_file = leanpub_remove_quiz(md_file)
  }

  x = readLines(md_file, warn = FALSE)
  image_lines = grep(x, pattern = "!\\[.*\\]\\((images.*)\\)")
  if (length(image_lines) > 0) {
    x[image_lines] = sub("\\(images", "(resources/images", x[image_lines])
  }
  image_lines = grep(x, pattern = "!\\[.*\\]\\((http*)\\)")
  if (length(image_lines) > 0) {
    x[image_lines] = sub("edit#slide=id.", "export/png?id=", x[image_lines],
                         fixed = TRUE)
  }
  xx = trimws(x)
  x = x[ !(xx %in% c("{format: png}", "{format: gif}"))]
  writeLines(x, con = md_file)
  md_file
}

#' @rdname leanpub_render
#' @param self_contained Produce a standalone HTML file,
#' passed to \code{\link{html_document}}
#' @param remove_tests Should the quizzes and exercises be removed
#' @export
leanpub_document = function(...,
                            remove_tests = TRUE,
                            self_contained = TRUE) {
  intermediates_generator <- function(input_file, encoding,
                                      intermediates_dir) {
    tfile = tempfile(tmpdir = intermediates_dir, fileext = ".md")
    file.copy(input_file, tfile)
    res = process_leanpub_md(tfile, remove_tests = remove_tests)
  }
  fmt <- rmarkdown::html_document(
    ...,
    self_contained = self_contained,
    intermediates_generator = intermediates_generator
  )
  fmt
}

leanpub_parse_indices = function(md_file, type = "quiz") {
  x = readLines(md_file)
  stub = basename(md_file)
  stub = sub("[.](R|)md$", "", stub)

  start_ind = grep(paste0("\\{", type), x)
  stop_ind = grep(paste0("\\{/", type), x)
  if (length(start_ind) == 0 & length(stop_ind) == 0) {
    return(NULL)
  }
  if (length(start_ind) != 1 & length(stop_ind) != 1) {
    stop("Multiple quiz chunks detected, stopping")
  }
  if (start_ind > stop_ind) {
    stop("{quiz} block after {/quiz} block")
  }
  return(list(start_ind = start_ind, stop_ind = stop_ind))
}


#' Parse Quiz from Markua Markdown File
#'
#' @param md_file Path to manuscript file
#'
#' @return A character vector of quiz
#' @export
#'
#' @examples
#' md_file = system.file("extdata", "00_template.md",
#'      package = "didactr")
#' if (file.exists(md_file)) {
#'    res = leanpub_parse_quiz(md_file)
#'    stopifnot(length(res) > 0)
#'    res = leanpub_parse_exercise(md_file)
#'    stopifnot(length(res) == 0)
#'    tfile = leanpub_remove_tests(md_file)
#'    res = leanpub_parse_quiz(tfile)
#'    stopifnot(length(res) == 0)
#' }
leanpub_parse_quiz = function(md_file) {

  res = leanpub_parse_indices(md_file, type = "quiz")

  if (is.null(res)) {
    return(NULL)
  }
  x = readLines(md_file)
  del_inds = seq(res$start_ind, res$stop_ind)
  quiz = x[del_inds]

  return(quiz)
}

#' @export
#' @rdname leanpub_parse_quiz
leanpub_parse_exercise = function(md_file) {

  res = leanpub_parse_indices(md_file, type = "exercise")

  if (is.null(res)) {
    return(NULL)
  }
  x = readLines(md_file)
  del_inds = seq(res$start_ind, res$stop_ind)
  quiz = x[del_inds]

  return(quiz)
}


leanpub_remover = function(
  md_file,
  test_type = c("quiz",
            "exercise")) {

  stopifnot(file.exists(md_file))

  stub = basename(md_file)
  stub = sub("[.](R|)md$", "", stub)
  tfile = file.path(tempdir(), basename(md_file))
  file.copy(md_file, tfile, overwrite = TRUE)
  for (type in test_type) {
    res = leanpub_parse_indices(tfile, type = type)
    if (!is.null(res)) {
      x = readLines(tfile)
      del_inds = seq(res$start_ind, res$stop_ind)
      x = x[-del_inds]
      writeLines(x, con = tfile)
    }
  }
  return(tfile)
}

#' @export
#' @rdname leanpub_parse_quiz
leanpub_remove_quiz = function(md_file) {
  stopifnot(file.exists(md_file))
  tfile = leanpub_remover(md_file, test_type = "quiz")
  return(tfile)
}


#' @export
#' @rdname leanpub_parse_quiz
leanpub_remove_exercise = function(md_file) {
  stopifnot(file.exists(md_file))
  tfile = leanpub_remover(md_file, test_type = "exercise")
  return(tfile)
}

#' @export
#' @rdname leanpub_parse_quiz
leanpub_remove_tests = function(md_file) {
  stopifnot(file.exists(md_file))
  tfile = leanpub_remover(
    md_file,
    test_type = c("quiz", "exercise"))
  return(tfile)
}


