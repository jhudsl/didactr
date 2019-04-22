
#' Starting a Course, Light wrapper for \code{\link{make_course}}
#'
#' @param course_name Name of the course.  Any spaces will be set to
#' underscores
#' @param root_path The directory to put the course into.
#' @param book_txt A text file with names of markdown
#'  files and course names
#' @param verbose print diagnostic messages
#' @param open 	If \code{TRUE}, activates the new project:
#' If RStudio desktop, the package is opened in a new session.
#' Otherwise, the working directory and active project is changed.
#' @param rstudio If \code{TRUE}, calls \code{use_rstudio()} to
#' make the new package or project into an RStudio Project.
#' @param git If \code{TRUE}, calls \code{use_git()} to
#' use Git inside the project.
#' @param ... additional argument to pass to \code{\link{create_lesson}}
#'
#' @importFrom usethis proj_set use_rstudio proj_activate create_project
#' @return The output of \code{\link{make_course}}.
#' @export
#' @examples
#' root_path = tempfile()
#' course_name = "test this out"
#' sc = start_course(course_name, root_path)
#'
#'
#' root_path = tempfile()
#' course_name = "test this out2"
#' book_txt =  system.file("extdata", "Book.txt", package = "didactr")
#' sc = start_course(course_name, root_path, book_txt = book_txt)
#' readLines(sc$book_txt)
#' md_files = list.files(sc$man_path, full.names = TRUE)
#' md_files
#' readLines(md_files[1])
start_course = function(course_name,
                        root_path = ".",
                        book_txt = NULL,
                        folder_id = NULL,
                        verbose = TRUE,
                        rstudio = TRUE,
                        open = FALSE,
                        git = FALSE,
                        ...){
  course_name = gsub(" ", "_", course_name)

  course_dir = file.path(root_path, course_name)
  dir.create(course_dir, showWarnings = FALSE, recursive = TRUE)


  old_project <- usethis::proj_set(course_dir, force = TRUE)
  on.exit(usethis::proj_set(old_project), add = TRUE)

  # usethis::create_project(path = course_dir, open = FALSE)

  res = make_course(course_dir = course_dir, book_txt = book_txt,
                    verbose = verbose, warn_book_exists = FALSE)


  if (!is.null(folder_id)) {
    df = gs_folder_df(id = folder_id, slides_only = TRUE)
    n = NROW(df)
    results = vector(length = n, mode = "list")
    for (i in seq(n)) {
      idf = df[i, , drop = FALSE]
      results[[i]] = create_lesson(
        lesson_name = idf$name,
        slide_id = idf$id,
        course_dir = res$course_dir,
        verbose = verbose,
        make_slide_deck = TRUE,
        ...
      )
    }
  }

  book_txt = readLines(res$book_txt, warn = FALSE)
  book_txt = trimws(book_txt)
  book_txt = book_txt[ book_txt != "" ]

  make_md_files = !all(file.exists(file.path(res$man_path, book_txt)))


  if (make_md_files) {
    if (length(book_txt) > 0) {
      lessons = create_lessons_from_book(
        course_dir = res$course_dir,
        verbose = verbose,
        ...)
      res$lessons = lessons
    }
  }


  if (rstudio) {
    rproj_file = file.path(course_dir, paste0(course_name, ".Rproj"))
    if (!file.exists(rproj_file)) {
      usethis::use_rstudio()
    }
    res$rproj_file = rproj_file
  }

  if (open) {
    if (usethis::proj_activate(course_dir)) {
      # Working directory/active project changed; so don't undo on exit
      on.exit()
    }
  }
  if (git) {
    usethis::use_git()
    msg = paste0("Run usethis::use_github() when your ",
                 "project opens to use GitHub")
    message(msg)
  }
  # make sure videos don't go up there.
  usethis::use_git_ignore("manuscript/resources/videos")

  return(res)
}

#' @export
#' @rdname start_course
create_course = start_course

#' @export
#' @rdname start_course
create_github_course = function(
  ...) {
  res = create_course(...)
  usethis::use_git()
  usethis::use_github()

  return(res)
}
