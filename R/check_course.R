#' Check Leanpub Course
#'
#' @param course_dir directory with course materials
#' @param save_metrics Should an `rds` file be saved of the `data.frame`?
#' @param timezone Timezone to be used?
#' @param require_authorization Should you authorize for Google
#' Slides and such to check the modified time?
#' @param use_book Use \code{Book.txt} to keep only the
#' manuscript files that were specified.
#' @param check_youtube_links Should YouTube links be checked?  If starting a
#' course, set to \code{FALSE}
#'
#'
#' @param ... arguments to pass to \code{\link{didactr_auth}}
#'
#' @return A list of data frames of the checked course. Will have
#' class \code{course_check}
#' @export
#' @importFrom googledrive drive_get
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom httr parse_url
#' @importFrom tidyr separate
#' @import dplyr
#' @examples
#' root_path = tempfile()
#' course_name = "test this out2"
#' book_txt =  system.file("extdata", "Book.txt", package = "didactr")
#' sc = create_course(course_name, root_path, book_txt = book_txt)
#' course_dir = sc$course_dir
#' in_ci <- function() {
#' nzchar(Sys.getenv("CI"))
#' }
#' if (!in_ci()) {
#' object = check_course(sc$course_dir, require_authorization = FALSE)
#' }
check_course = function(
  course_dir = ".",
  save_metrics = TRUE,
  timezone = "America/New_York",
  require_authorization = TRUE,
  use_book = FALSE,
  check_youtube_links = TRUE,
  ...) {

  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("stringr required for check_course")
  }
  lesson_name = gs_name = drive_resource = NULL
  rm(list = c("lesson_name", "gs_name", "drive_resource"))

  scr_para_length = yt_md_link = vid_file = NULL
  rm(list = c("scr_para_length", "yt_md_link", "vid_file"))

  mod_time_vid = mod_time_scr = lesson = NULL
  rm(list = c("mod_time_vid", "mod_time_scr", "lesson"))

  n_slides = pdf_pages = n_pngs = img_dir = lesson = NULL
  rm(list = c("img_dir", "lesson", "pdf_pages", "n_pngs", "n_slides"))

  mod_time_pngs = gs_more_recent = NULL
  rm(list = c("mod_time_pngs", "gs_more_recent"))

  mod_time_gs = scr_file = has_scr_file = NULL
  rm(list = c("scr_file", "has_scr_file", "mod_time_gs"))


  paths = check_structure(course_dir)
  ######################################
  # get manuscript md files and check names of
  ######################################
  manuscript_files = list.files(
    pattern = ".md$",
    path = paths$man_path,
    full.names = TRUE)

  if (use_book) {
    book_txt = readLines(paths$book_txt)
    book_txt = trimws(book_txt)
    manuscript_files = manuscript_files[
      basename(manuscript_files) %in% book_txt]
  }
  man_stubs = sub("[.]md$", "", basename(manuscript_files))


  # md file has highest precedence
  df = tibble::tibble(lesson = man_stubs, md_file = manuscript_files)

  # create a title lesson name
  df = df %>%
    mutate(lesson_name = stringr::str_replace(lesson, "^\\d+_", ""),
           lesson_name = stringr::str_replace_all(lesson_name, "_", " "),
           lesson_name = stringr::str_to_title(lesson_name))

  ## get IDs for Slides
  df$id = sapply(df$md_file, gs_id_from_slide)
  if (any(is.na(df$id))) {
    message(
      paste0("Google Slides ID is missing from following lessons: ",
             paste0(df$lesson[is.na(df$id)], collapse = ", ")))
  }

  if (require_authorization) {
    authorized = check_didactr_auth(...)
  } else {
    authorized = is_didactr_authorized()
  }

  ######################################
  ## Get information from Google Drive
  ######################################
  d <- df %>%
    filter(!is.na(id))
  if (nrow(d) > 0 && authorized) {
    drive_info = drive_information(id = d$id, timezone = timezone)
    if (!is.null(drive_info)) {
      drive_info$n_slides = sapply(drive_info$id, function(id) {
        nrow(gs_slide_df(id))
      })
      df = left_join(df, drive_info, by = "id")
      df = distinct(df)
    }
  } else {
    df$gs_name = NA
    df$mod_time_gs = NA
    df$n_slides = NA
  }
  ######################################
  ## make image paths
  ######################################
  df = df %>%
    mutate(img_dir = file.path(paths$img_path, lesson),
           has_img_dir = dir.exists(img_dir))

  if (any(!df$has_img_dir)) {
    sapply(df$img_dir[!df$has_img_dir],
           dir.create,
           recursive = TRUE,
           showWarnings = FALSE)
  }
  df = df %>%
    mutate(has_img_dir = dir.exists(img_dir))

  ######################################
  # Not correct GS ID for MD files
  ######################################
  if (anyDuplicated(df$id)) {
    dup_df = df %>%
      filter(!is.na(id)) %>%
      dplyr::group_by(id) %>%
      dplyr::add_tally() %>%
      dplyr::filter(n > 1)
    if (nrow(dup_df) > 0) {
      warning(paste0("Duplicated IDs (slideshow links) ",
                     "are present!  MD files are off"))
      print(dup_df)
    }
  }



  ######################################
  # this returns the actual links in the text
  ######################################
  image_links = lapply(df$md_file, get_image_link_from_slide)
  names(image_links) <- df$lesson

  ######################################
  # this returns the actual image filenames referenced
  # we will check to see if all images referenced exist
  ######################################
  images = lapply(df$md_file, get_image_from_slide)
  names(images) <- df$lesson

  df$all_images_exist = sapply(images, function(x) {
    all(file.exists(file.path(paths$res_path, x)))
  })

  # naming conventions for the images folders
  img_dirs = list.dirs(
    path = paths$img_path, recursive = FALSE,
    full.names = TRUE)
  # check if image directories exist but don't have MD file
  bad_img_dir = !(img_dirs %in% df$img_dir)
  if (any(bad_img_dir)) {
    warning(paste0("An image directory exists but doesn't correspond to a ",
                   "lesson.  Possible naming inconsistency? Or old directories
                   that need to be deleted?"))
    cat(img_dirs[bad_img_dir], sep = "\n")
  }
  # names(bad_img_dir) <- df$lesson

  # Check if a image folder has a PDF
  df$pdf = sapply(df$img_dir, list_one_file, ending = "pdf")

  # Check the number of pages of the pdf to cross-ref with the pngs
  # n_pdf_pages is function in didactr
  df$pdf_pages = sapply(df$pdf, n_pdf_pages)

  # list out the pngs of the folder
  png_names = lapply(df$img_dir, list.files,
                     pattern = "[.]png")
  df$n_pngs = sapply(png_names, length)
  df = df %>%
    mutate(n_pngs = ifelse(n_pngs == 0 & !is.na(n_slides),
                           n_slides, n_pngs)
    )

  # need this because of NA
  # setting those to FALSE
  df = df %>%
    mutate(pdf_png_match = na_false(pdf_pages == n_pngs))


  ## get mtime for each lesson
  ## if no pngs exist, NA
  ## to be used to see if slides have been updated more recently
  ## (images should then be re-rendered)
  mod_files = list.files(
    pattern = "-(0|)1.png",
    path = file.path(paths$img_path, df$lesson),
    full.names = TRUE)
  mod_times = bind_cols(
    lesson = basename(dirname(mod_files)),
    mod_time_pngs = mod_time_to_tz_time(mod_files, timezone = timezone))

  df = df %>%
    left_join(mod_times, by = "lesson") %>%
    mutate(gs_more_recent = ifelse(is.na(mod_time_pngs), TRUE, mod_time_gs > mod_time_pngs),
           gs_more_recent = ifelse(is.na(gs_name), NA, gs_more_recent))

  ## get script path with correct directory names
  df = df %>%
    mutate(scr_file = file.path(paths$scr_path,
                                paste0(lesson, "_script.md")))

  # naming conventions for the images folders
  scr_files = list.files(path = paths$scr_path, recursive = FALSE,
                         full.names = TRUE)
  names(scr_files) = scr_files

  ## make sure expected script file is there
  df = df %>%
    mutate(has_scr_file = file.exists(scr_file),
           scr_file = ifelse(has_scr_file, scr_file, NA_character_))

  ## get length of script file
  ## check to see if length of script file matches number of pngs
  df = df %>%
    mutate(scr_para_length = sapply(scr_file, n_para)) %>%
    mutate(
      scr_png_match = na_false(scr_para_length == n_pngs),
      mod_time_scr =  mod_time_to_tz_time(scr_file, timezone = timezone))


  youtube_link_in_md = function(fname, check_youtube_links = TRUE) {
    x = readLines(fname, warn = FALSE)
    # will find better singular regex for this eventually...
    line <- grep(pattern = yt_pattern(),
                 x, perl = TRUE) #
    if (length0(line)) {
      return(NA)
    }
    #remove gifs
    line <- line[!grepl("gif", x[line])]
    x = sub("(^!\\[.+\\]\\()(.+)(\\))","\\2",x[line])
    # remove images
    x = x[!startsWith(x, "images")]
    # remove images if accidentally with resources
    x = x[!startsWith(x, "resources/images")]
    x = x[!grepl(pattern = "docs[.]google[.]com/", x)]
    if (length0(x)) {
      return(NA)
    }
    res = startsWith(x, "!")
    if (any(res)) {
      x[res] <- NA
    }
    x = length0_to_NA(x)
    if (length(x) > 1) {
      has_you = grepl("you", x)
      if (sum(has_you) == 1) {
        x = x[has_you]
      } else {
        msg = paste0("MULTIPLE LINES found for Youtube Link ", fname,
                     ", keeping first")
        message(msg)
        print(x)
        warning(msg)
        x = x[1]
      }
    }
    if (!is.na(x)) {
      if (!grepl("you", x)) {
        if (check_youtube_links) {
          msg = paste0("Youtube Link doesn't contain YOU for:\n", fname, "\n")
          message(msg)
          print(x)
          warning(msg)
        }
      }
    }
    return(x)
  }


  ## Get YouTube Links currently in the markdown file
  df$yt_md_link = unlist(sapply(df$md_file, youtube_link_in_md,
                                check_youtube_links = check_youtube_links))


  ## make sure expected vid file is there
  df = df %>%
    mutate(has_vid_link = grepl("youtu", yt_md_link))

  # get video path with correct video
  # get manuscript md files and check names of
  vid_files = list.files(pattern = ".mp4$", path = paths$vid_path,
                         full.names = TRUE)
  vid_stubs = sub("[.]mp4$", "", basename(vid_files))
  vid_df = bind_cols(vid_file = vid_files, lesson = vid_stubs)

  df = df %>%
    left_join(vid_df, by = "lesson")

  ##############
  # Stopped here
  ##############


  ## make sure expected vid file is there
  df = df %>%
    mutate(
      mod_time_vid = mod_time_to_tz_time(vid_file, timezone = timezone),
      has_vid_file = file.exists(vid_file),
      vid_more_recent = ifelse(is.na(mod_time_vid), TRUE, mod_time_pngs > mod_time_vid),
      scr_more_recent = ifelse(is.na(has_scr_file), TRUE , mod_time_scr > mod_time_vid)
    )


  ## Get youtube IDs
  df$yt_md_ID = sapply(
    df$md_file,
    function(fname) {
      x = readLines(fname, warn = FALSE)
      line <-
        grep(pattern = yt_pattern(), x, perl = TRUE)
      line <-
        line[grep("gif", x[line], invert = TRUE)]
      ## get youtube ID
      ## this will break if youtube ever decides
      ## to change the length of their IDs
      x = ifelse(!is.na(df$yt_md_link[df$md_file == fname]),
                 stringr::str_sub(x[line], -12, -2), NA)
    })
  course_status = df
  if (save_metrics) {
    saveRDS(course_status,
            file = file.path(paths$met_path, "course_status.rds"),
            compress = "xz")
  }
  L = list(course_summary = df, images = images,
           image_links = image_links, bad_img_dir = bad_img_dir,
           course_dir = course_dir)
  L$paths = paths
  L$save_metrics = save_metrics
  class(L) = "course_check"
  summary(L)
  return(L)
}


