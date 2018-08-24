
#' Make a Lesson
#'
#' @param lesson_name Name of the Lesson to create.  This should be
#' human-readable and will be the title of your lesson in your course.
#' @param course_dir directory with course materials
#' @param verbose print diagnostic messages
#' @param md_file Output markdown file to create.  If not specified,
#' will take the \code{lesson_name}, sub out spaces, lower case it,
#' and use that for the file name.
#' @param slide_id ID to slide deck on Google Slides.
#' @param make_slide_deck Create a slide deck on Google Slides if
#' no link is provided.
#'
#' @return A list of the created markdown manuscript file and script files.
#' \code{make_lessons_from_book} will return a list of these lists,
#' one for each lesson in the \code{Book.txt} file.
#' @export
#'
#' @examples
#' root_path = tempfile()
#' course_name = "test"
#' sc = start_course(course_name, root_path)
#' verbose = TRUE
#' out =make_lesson(lesson_name = "how to Do Things", course_dir = sc$course_dir)
#' dir(sc$man_path)
#' dir(sc$scr_path)
#' readLines(sc$book_txt)
#' readLines(out$md_file)
#'
#' \donttest{
#' root_path = tempfile()
#' course_name = "test"
#' sc = start_course(course_name, root_path)
#' verbose = TRUE
#' out = make_lesson(lesson_name = "how to Do Things",
#' course_dir = sc$course_dir,
#' make_slide_deck = TRUE)
#' dir(sc$man_path)
#' dir(sc$scr_path)
#' readLines(sc$book_txt)
#' readLines(out$md_file)
#' }
#'
#' root_path = tempfile()
#' course_name = "test"
#' sc = start_course(course_name, root_path)
#' verbose = TRUE
#' x =  system.file("extdata", "Book.txt", package = "didactr")
#' file.copy(x, sc$book_txt, overwrite = TRUE)
#' course_dir = sc$course_dir
#' res = make_lessons_from_book(course_dir = sc$course_dir)
#' dir(sc$man_path)
#' dir(sc$scr_path)
#' readLines(sc$book_txt)
#' readLines(res[[3]]$md_file)
make_lesson = function(
  lesson_name,
  course_dir = ".",
  verbose = TRUE,
  md_file = NULL,
  make_slide_deck = FALSE,
  slide_id = NULL) {

  template_file = system.file("extdata", "00_template.md", package = "didactr")
  template = readLines(template_file)
  template = gsub("Lesson Name", lesson_name, template, fixed = TRUE)

  res = make_course(course_dir = course_dir, verbose = verbose)
  book_txt = readLines(res$book_txt)
  book_txt = book_txt[ book_txt != ""]

  if (is.null(md_file)) {
    file_name = tolower(lesson_name)
    file_name = gsub(" ", "_", file_name)

    # number it
    number = sprintf("%02.0f", length(book_txt))

    # add in quiz name and such
    stub = paste0(number, "_", file_name)

    file_name = paste0(stub, ".md")
  } else {
    md_file = trimws(md_file)
    stopifnot(grepl("md$", md_file))
    stub = sub(".md$", "", md_file)
    file_name = md_file
  }

  template = gsub("00_filename", stub, template)


  slide_url = function(slide_id) {
    paste0("https://docs.google.com/presentation/d/",
           slide_id,
           "/edit?usp=sharing")
  }
  # if not missing slide id
  if (!is.null(slide_id)) {
    template = gsub("Link to Slides", slide_url(slide_id),
                    template, fixed = TRUE)
  } else {
    if (make_slide_deck) {
      # authorize
      check_didactr_auth()

      # make the name
      # naming convention changed to course_NUMBER_lesson
      gs_name = paste0(res$course_name, "_",
                       stub)
      # see if it exists
      id_exists = googledrive::drive_find(
        pattern = gs_name, type = "presentation",
        n_max = 25)
      # if so, make it the slide_id variable
      slide_id = id_exists$id[1]
      # if not, it will be set to NA
      if (is.na(slide_id)) {

        get_req = googledrive::drive_get(
          id = "143gvqcynq_bl7iVd2G9yjumwJJkAy0S6CyNCsrJ2LgE")
        slide_id = googledrive::drive_cp(get_req,
                                         path = gs_name,
                                         verbose = verbose)
        slide_id = slide_id$id
      }
      # if we have a slide id (either copied or already exists)
      # paste it in
      if (!is.na(slide_id)) {
        template = gsub("Link to Slides", slide_url(slide_id),
                        template, fixed = TRUE)
      }

    }
  }

  md_file_name = file_name
  file_name = file.path(res$man_path, file_name)
  if (file.exists(file_name)) {
    warning("File exists, will not overwrite!")
  } else {
    writeLines(template, con = file_name)
  }

  script_name = paste0(stub, "_script.md")
  script_name = file.path(res$scr_path, script_name)
  if (file.exists(script_name)) {
    warning("Script file exists, will not overwrite!")
  } else {
    writeLines("", con = script_name)
  }

  check = grepl(stub, book_txt)
  if (length(book_txt) == 0 || !any(check)) {
    book_txt = c(book_txt, md_file_name)
    writeLines(book_txt, res$book_txt)
  }

  L = list(md_file = file_name,
           script_file = script_name)
  return(L)
}

#' @rdname make_lesson
#' @param ... additional arguments to pass to
#' \code{\link{make_lesson}}
#' @export
make_lessons_from_book = function(
  course_dir = ".",
  verbose = TRUE,
  ...) {

  md_file = txt = lesson_name = have_name = NULL
  rm(list = c("md_file", "txt", "lesson_name", "have_name"))

  res = make_course(course_dir = course_dir, verbose = verbose)
  book_txt = readLines(res$book_txt, warn = FALSE)
  book_txt = trimws(book_txt)
  book_txt = book_txt[ book_txt != ""]

  if (length(book_txt) == 0) {
    stop("Book.txt is empty!  No lessons to create")
  }
  df = data_frame(txt = book_txt)
  df = df %>%
    mutate(have_name = grepl("#", txt),
           md_file = sub("(.*)#(.*)", "\\1", txt),
           md_file = gsub("#", "", md_file),
           md_file = trimws(md_file),
           lesson_name = ifelse(have_name, sub(".*#(.*)", "\\1", txt),
                                sub(".md$", "", trimws(txt))),
           lesson_name = trimws(lesson_name),
           lesson_name = ifelse(
             !have_name,
             sub("^\\d*_", "", lesson_name),
             lesson_name),
           lesson_name = ifelse(
             !have_name,
             gsub("_", " ", lesson_name),
             lesson_name)
    )
  # capitalize first letter
  x = df$lesson_name[ !df$have_name]
  substr(x, 1,1) = toupper(substr(x, 1,1))
  df$lesson_name[ !df$have_name] = x

  results = mapply(function(lesson_name, md_file) {
    make_lesson(lesson_name = lesson_name,
                course_dir = res$course_dir,
                verbose = verbose,
                md_file = md_file,
                ...)

  }, df$lesson_name, df$md_file, SIMPLIFY = FALSE)

  return(results)
}
