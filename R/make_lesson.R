
#' Create a Lesson
#'
#' @param lesson_name Name of the Lesson to create.  This should be
#' human-readable and will be the title of your lesson in your course.
#' @param course_dir directory with course materials
#' @param verbose print diagnostic messages
#' @param md_file Output markdown file to create.  If not specified,
#' will take the \code{lesson_name}, sub out spaces, lower case it,
#' and use that for the file name.
#' @param slide_id ID to slide deck on Google Slides.  Caution,
#' this will publish the slide deck to the web.
#' @param make_slide_deck Create a slide deck on Google Slides if
#' no link is provided.
#' @param rmd Should an Rmd (Rmarkdown) be made versus a Markdown?
#' @param open should the file be opened for editing?
#' @param extract_code If you have text with \code{#rstats} in the
#' slide or \code{#rstats} in the Alt-text title, code will be
#' included
#' @param template_slide_id Slide ID for a template.  This template
#' will be copied to your new slide deck.
#' @param publish should the slide be published so slides can be
#' included in the markdown?
#' @param add_number Add a prefix to the file name, such as
#' `00_` for the first file
#' @param ... arguments passed to \code{\link{check_didactr_auth}}
#'
#' @return A list of the created markdown manuscript file and script files.
#' \code{create_lessons_from_book} will return a list of these lists,
#' one for each lesson in the \code{Book.txt} file.
#' @export
#'
#' @importFrom googledrive as_id drive_share drive_cp drive_get
#'
#' @examples
#' root_path = tempfile()
#' course_name = "test"
#' sc = start_course(course_name, root_path)
#' verbose = TRUE
#' out =create_lesson(lesson_name = "how to Do Things",
#' course_dir = sc$course_dir,
#' make_slide_deck = FALSE,
#' open = FALSE)
#' dir(sc$man_path)
#' dir(sc$scr_path)
#' readLines(sc$book_txt, warn = FALSE)
#' readLines(out$md_file)
#' in_ci <- function() {
#' nzchar(Sys.getenv("CI"))
#' }
#' \dontrun{
#' # requires authorization
#' root_path = tempfile()
#' course_name = "test"
#' sc = start_course(course_name, root_path)
#' verbose = TRUE
#' out = create_lesson(lesson_name = "how to Do Things",
#' course_dir = sc$course_dir,
#' make_slide_deck = !in_ci(),
#' open = interactive())
#' dir(sc$man_path)
#' dir(sc$scr_path)
#' readLines(sc$book_txt, warn = FALSE)
#' readLines(out$md_file, warn = FALSE)
#' }
#'
#' root_path = tempfile()
#' course_name = "test"
#' sc = start_course(course_name, root_path)
#' verbose = TRUE
#' x =  system.file("extdata", "Book.txt", package = "didactr")
#' file.copy(x, sc$book_txt, overwrite = TRUE)
#' course_dir = sc$course_dir
#' res = create_lessons_from_book(course_dir = sc$course_dir, open = FALSE,
#' make_slide_deck = FALSE
#' )
#' dir(sc$man_path)
#' dir(sc$scr_path)
#' readLines(sc$book_txt)
#' readLines(res[[3]]$md_file)
create_lesson = function(
  lesson_name,
  course_dir = ".",
  verbose = TRUE,
  md_file = NULL,
  make_slide_deck = TRUE,
  slide_id = NULL,
  extract_code = FALSE,
  rmd = extract_code,
  open = FALSE,
  template_slide_id = "143gvqcynq_bl7iVd2G9yjumwJJkAy0S6CyNCsrJ2LgE",
  publish = TRUE,
  add_number = TRUE,
  ...) {

  ext = ifelse(rmd, ".Rmd", ".md")
  fname = paste0("00_template", ext)
  template_file = system.file("extdata", fname, package = "didactr")
  template = readLines(template_file, warn = FALSE)
  template = gsub("%20", " ", template)
  template = gsub("Lesson Name", lesson_name, template, fixed = TRUE)

  course = make_course(course_dir = course_dir, verbose = verbose)
  book_txt = readLines(course$book_txt, warn = FALSE)
  book_txt = book_txt[ book_txt != ""]

  if (!is.null(slide_id)) {
    if (publish | make_slide_deck) {
      check_didactr_auth(...)
    }
  }

  ask_overwrite = function() {
    choices = c("No", "Yes")
    if (interactive()) {
      ask_res = utils::menu(
        choices = choices,
        title = "A file name looks similar, should we continue")
    } else {
      ask_res = 2
    }
    return(ask_res)
  }
  if (is.null(md_file)) {
    file_name = tolower(lesson_name)
    file_name = gsub(" ", "_", file_name)

    no_under_book = sub("^\\d{2}_", "", book_txt)
    no_under_book = sub("[.]md$", "", no_under_book)
    if (any(no_under_book %in% file_name)) {
      ask_res = ask_overwrite()
      if (ask_res != 2) {
        return(NULL)
      }
    }
    if (add_number) {
      # number it
      number = sprintf("%02.0f", length(book_txt))

      # add in quiz name and such
      stub = paste0(number, "_", file_name)
    } else {
      stub = file_name
    }

    file_name = paste0(stub, ext)
  } else {
    md_file = trimws(md_file)
    stopifnot(grepl("md$", md_file))
    stub = sub(".md$", "", md_file)
    file_name = md_file
    if (file.exists(md_file)) {
      ask_res = ask_overwrite()
      if (ask_res != 2) {
        return(NULL)
      }
    }
  }

  template = gsub("00_filename", stub, template)

  # if not missing slide id

  script = ""
  # add in the PNG links
  L = create_slide_deck(
    slide_id = slide_id,
    template = template,
    publish = publish,
    make_slide_deck = make_slide_deck,
    course = course,
    stub = stub,
    template_slide_id = template_slide_id,
    verbose = verbose)
  template = L$template
  slide_id = L$slide_id
  rm(L)
  template = extract_notes(
    slide_id = slide_id,
    template = template,
    extract_code = extract_code)

  md_file_name = sub("[.]Rmd", ".md", file_name)
  file_name = file.path(course$man_path, file_name)
  if (file.exists(file_name)) {
    warning("File exists, will not overwrite!")
  } else {
    writeLines(template, con = file_name)
  }

  script_name = paste0(stub, "_script.md")
  script_name = file.path(course$scr_path, script_name)
  if (file.exists(script_name)) {
    warning("Script file exists, will not overwrite!")
  } else {
    writeLines(script, con = script_name, sep = "\n\n")
  }

  check = grepl(stub, book_txt)
  if (length(book_txt) == 0 || !any(check)) {
    book_txt = c(book_txt, md_file_name)
    writeLines(book_txt, course$book_txt)
  }

  if (open) {
    usethis::edit_file(file_name)
    usethis::edit_file(script_name)
  }
  L = list(md_file = file_name,
           script_file = script_name,
           course_dir = course_dir)
  L$slide_id = slide_id
  return(L)
}

#' @rdname create_lesson
#' @export
create_lessons_from_book = function(
  course_dir = ".",
  verbose = TRUE,
  make_slide_deck = FALSE,
  ...) {

  md_file = txt = lesson_name = have_name = NULL
  rm(list = c("md_file", "txt", "lesson_name", "have_name"))

  course = make_course(course_dir = course_dir, verbose = verbose)
  book_txt = readLines(course$book_txt, warn = FALSE)
  book_txt = trimws(book_txt)
  book_txt = book_txt[ book_txt != ""]

  if (length(book_txt) == 0) {
    stop("Book.txt is empty!  No lessons to create")
  }
  df = dplyr::tibble(txt = book_txt)
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
    create_lesson(
      lesson_name = lesson_name,
      course_dir = course$course_dir,
      verbose = verbose,
      md_file = md_file,
      make_slide_deck = make_slide_deck,
      ...)
  }, df$lesson_name, df$md_file, SIMPLIFY = FALSE)

  return(results)
}


extract_notes = function(slide_id, template, extract_code) {
  if (!is.null(slide_id)) {
    slide_df = gs_slide_df(slide_id)
    notes = notes_from_slide_output(slide_df)
    markdown_pngs = slide_df$png_markdown
    code = slide_df$code
    code[ is.na(code) ] = ""
    # add the notes to the
    script = notes
    script[ script %in% ""] = ";"
    script = gsub("\n", " ", script)
    notes = paste0("<!-- Notes from Slide ", slide_df$objectId, "-->\n",
                   notes, "\n")
    if (extract_code) {
      notes = paste0(notes, code, "\n")
    }
    # markdown_pngs = c(t(cbind(notes, markdown_pngs)))
    markdown_pngs = paste0(notes, markdown_pngs)
    ind = grep("SLIDESHERE", template, fixed = TRUE)
    stopifnot(length(ind) == 1)
    template = c(template[1:(ind - 1)],
                 markdown_pngs,
                 template[(ind + 1):length(template)])
    template = gsub("SLIDEID", slide_id, template)
  }
  template
}


create_slide_deck = function(
  slide_id,
  template,
  publish,
  make_slide_deck,
  course,
  stub,
  template_slide_id,
  verbose) {

  slide_url = function(slide_id) {
    paste0("https://docs.google.com/presentation/d/",
           slide_id,
           "/edit?usp=sharing")
  }

  if (!is.null(slide_id)) {
    if (
      any(
        grepl("^http", slide_id) |
        grepl("google.com", slide_id)
      )
    ) {
      slide_id = didactr::get_slide_id(slide_id)
    }
    template = gsub("Link to Slides", slide_url(slide_id),
                    template, fixed = TRUE)
    if (verbose) {
      message("Publishing Slide")
    }
    if (publish) {
      googledrive::drive_share(
        file = googledrive::as_id(slide_id),
        verbose = verbose,
        type = "anyone")
    }
  } else {
    if (make_slide_deck) {
      # make the name
      # naming convention changed to course_NUMBER_lesson
      gs_name = paste0(course$course_name, "_",
                       stub)
      # see if it exists
      id_exists = googledrive::drive_find(
        pattern = gs_name,
        type = "presentation",
        n_max = 25)
      if (nrow(id_exists) > 0) {
        warning(paste0("Slide deck with name ", gs_name, " already exists"))
        print(id_exists)
      }
      if (is.null(slide_id)) {
        slide_id = NA
      }
      # if so, make it the slide_id variable
      # slide_id = id_exists$id[1]

      # if not, it will be set to NA
      if (is.na(slide_id)) {

        # this is a public slide deck I believe
        # need to try with other OAuth account
        get_req = googledrive::drive_get(id = template_slide_id)
        if (verbose) {
          message("Copying over lecture")
        }
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
        if (publish) {
          googledrive::drive_share(
            file = googledrive::as_id(slide_id),
            verbose = verbose,
            type = "anyone")
        }
      } else {
        slide_id = NULL
      }

    }
  }
  list(template = template,
       slide_id = slide_id)
}
