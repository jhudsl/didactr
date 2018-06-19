#' Check Leanpub Course
#'
#' @param course_dir directory with course materials
#' @param save_metrics Should an `rds` file be saved of the `data.frame`?
#' @param timezone Timezone to be used?
#'
#' @return A data frame of the checked course.
#' @export
#' @importFrom googledrive drive_get
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom stringr str_sub
#' @importFrom httr parse_url
#' @importFrom tidyr separate
#' @import dplyr
#'
check_course = function(course_dir = ".", save_metrics = TRUE,
                        timezone = "America/New_York") {
  paths = check_structure(course_dir)
  # get manuscript md files and check names of
  manuscript_files = list.files(
    pattern = ".md$",
    path = paths$man_path,
    full.names = TRUE)

  man_stubs = sub("[.]md$", "", basename(manuscript_files))
  # md file has highest precedence
  df = data_frame(lesson = man_stubs, md_file = manuscript_files)


  ## get IDs for Slides
  df$id = sapply(df$md_file, function(fname) {
    x = readLines(fname, warn = FALSE)
    x = grep(x, pattern = "\\[(S|s)lides\\]", value = TRUE)
    x = sub(".*\\((http.*)\\).*", "\\1", x)
    x = unlist(sapply(x, function(r) parse_url(r)$path))
    x = sub("/edit$", "", x)
    x = basename(x)
    x = unique(x)
    if (length(x) > 1) {
      warning(paste0("Multiple sheets identified!  Please check ",
                     fname))
    }
    if (length(x) == 0 || grepl("\\(\\)", x)) {
      return(NA)
    }
    return(x)
  })

  d <- sapply(df$id, function(x) {
    if(is.na(x)){
      message("Google Slides ID is missing from markdown lesson df$lesson[df$ID==x]")
    }})

  ## Get information from Google Drive
  d <- df %>% filter(!is.na(df$id))
  drive_info = drive_get(id = d$id)
  if (nrow(drive_info) > 0) {
    drive_info = drive_info %>%
      rename(gs_name = name) %>%
      mutate(course_info=gs_name)
    # %>%
    #   separate(col=course_info, sep = "_",
    #            into=c("cnum", "course","lesson_name"),
    #            extra="merge")
    mod_time_gs = sapply(drive_info$drive_resource,
                         function(x) {
                           x$modifiedTime
                         })
    drive_info$mod_time_gs = ymd_hms(mod_time_gs)
    drive_info$mod_time_gs = lubridate::with_tz(drive_info$mod_time_gs, tz = timezone)
    drive_info = drive_info %>%
      select(-drive_resource)
    df = left_join(df, drive_info, by = "id")
    df = distinct(df)
  }

  ## get image path with correct directory names
  df = df %>%
    mutate(img_dir = file.path(paths$img_path, lesson))

  a <- sapply(df$img_dir,
              function(x){
                if(!dir.exists(x)){
                  message(paste0("Creating image directories for: ", df$lesson[df$img_dir==x]))
                  dir.create(x, showWarnings = FALSE)

                }})

  # naming conventions for the images folders
  img_dirs = list.dirs(path = paths$img_path, recursive = FALSE,
                       full.names = TRUE)
  names(img_dirs) = img_dirs

  df = df %>%
    mutate(has_img_dir = img_dir %in% img_dirs)

  if (anyDuplicated(df$id)) {
    dup_df = df %>%
      group_by(id) %>%
      add_tally() %>%
      filter(n > 1)
    warning("Duplicated IDs (slideshow links) are present!  MD files are off")
    print(dup_df)
  }


  image_links = lapply(df$md_file, function(fname) {
    x = readLines(fname, warn = FALSE)
    x = grep(x, pattern = "!\\[.*\\]\\((images.*)\\)", value = TRUE)
    x = sub(x, pattern = "!\\[(.*)\\]\\((images.*)\\)", replacement = "\\1")
    return(x)
  })
  names(image_links) <- df$lesson


  ## check to see if all images referenced exist
  images = lapply(df$md_file, function(fname) {
    x = readLines(fname, warn = FALSE)
    x = grep(x, pattern = "!\\[.*\\]\\((images.*)\\)", value = TRUE)
    x = sub(x, pattern = "!\\[.*\\]\\((images.*)\\)", replacement = "\\1")
    return(x)
  })

  names(images) <- df$lesson

  df$all_images_exist = sapply(images, function(x) {
    all(file.exists(file.path(paths$res_path, x)))
  })

  # check if image directories exist but don't have MD file
  bad_img_dir = !(img_dirs %in% df$img_dir)
  if (any(bad_img_dir)) {
    warning(paste0("An image directory exists but doesn't correspond to a ",
                   "lesson.  Possible naming inconsistency? Or old directories
                   that need to be deleted?"))
    cat(img_dirs[bad_img_dir], sep = "\n")
  }
  names(bad_img_dir) <- df$lesson

  # Check if a image folder has a PDF
  df$pdf = sapply(df$img_dir,
                  function(x) {
                    pdfs = list.files(pattern = "[.]pdf",
                                      path = x,
                                      full.names = TRUE)
                    if (length(pdfs) > 1) {
                      warning(paste0(paths$img_path, " had more than one PDF! ",
                                     "Only grabbing first"))
                      pdfs = pdfs[1]
                    }
                    if (length(pdfs) == 0) {
                      return(NA)
                    }
                    return(pdfs)
                  })

  # Check the number of pages of the pdf to cross-ref with the pngs
  # n_pdf_pages is function in didactr
  df$pdf_pages = sapply(df$pdf, n_pdf_pages)

  # list out the pngs of the folder
  png_names = lapply(df$img_dir,
                     function(x) {
                       pngs = list.files(pattern = "[.]png",
                                         path = x)
                       pngs
                     })
  df$n_pngs = sapply(png_names, length)

  df = df %>%
    mutate(pdf_png_match = ifelse(pdf_pages == n_pngs, TRUE, FALSE))

  mod_time_to_tz_time = function(x, timezone) {
    mod_times = file.info(x)$mtime
    mod_times = ymd_hms(mod_times, tz = Sys.timezone())
    mod_times = lubridate::with_tz(mod_times, tz = timezone)
    return(mod_times)
  }
  ## get mtime for each lesson
  ## if no pngs exist, NA
  ## to be used to see if slides have been updated more recently
  ## (images should then be re-rendered)
  mod_files = list.files(
    pattern = "-1.png",
    path = file.path(paths$img_path, df$lesson),
    full.names = TRUE)
  mod_times = bind_cols(lesson = basename(dirname(mod_files)),
                        mod_time_pngs = mod_time_to_tz_time(mod_files, timezone = timezone))

  df = df %>%
    left_join(mod_times, by = "lesson") %>%
    mutate(gs_more_recent = ifelse(is.na(mod_time_pngs),TRUE, mod_time_gs > mod_time_pngs))


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
    mutate(has_scr_file = scr_file %in% scr_files)

  # get script path and number of paragraphs
  get_para <- function(x){
    para = readLines(x, warn = FALSE)
    para = para[ !para %in% ""]
    return(length(para))
  }

  ## get length of script file
  ## check to see if length of script file matches number of pngs
  df = df %>%
    mutate(scr_para_length = ifelse(has_scr_file == FALSE, NA,
                                    sapply(scr_file,get_para))) %>%
    mutate(
      scr_png_match = ifelse(scr_para_length == n_pngs, TRUE, FALSE),
      mod_time_scr =  mod_time_to_tz_time(scr_file, timezone = timezone),
      scr_more_recent = ifelse(is.na(has_scr_file), NA , mod_time_gs > mod_time_scr))


  ## Get YouTube Links currently in the markdown file
  yt_md_link = sapply(df$md_file,
                                function(fname) {
                                  x = readLines(fname, warn = FALSE)
                                  # will find better singular regex for this eventually...
                                  line <- grep(pattern = "^!\\[.+\\]\\((?!\\.png)\\)|^!\\[\\]\\((?!\\.png)\\)|^!\\[.+\\]\\((?!\\.png)\\)|!\\[.+\\]\\(.+[^.png]\\)|^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)", x, perl=TRUE) #
                                  x = sub("(^!\\[.+\\]\\()(.+)(\\))","\\2",x[line])
                                  # remove images
                                  x = x[!startsWith(x, "images")]
                                  if (length(x) < 1) {
                                    x <- NA
                                  }
                                  res = startsWith(x, "!")
                                  if (any(res)) {
                                    x[res] <- NA
                                  }
                                  return(x)
                                })

  df$yt_md_link = unlist(yt_md_link)

  ## make sure expected vid file is there
  df = df %>%
    mutate(has_vid_link = grepl("youtu",yt_md_link))

  # get video path with correct video
  # get manuscript md files and check names of
  vid_files = list.files(pattern = ".mp4$", path = paths$vid_path,
                         full.names = TRUE)


  vid_stubs = sub("[.]mp4$", "", basename(vid_files))
  vid_df = bind_cols(vid_file = vid_files, vid_stubs = vid_stubs)

  df = df %>%
    left_join(vid_df, by=c("lesson"="vid_stubs"))

  ## make sure expected vid file is there
  df = df %>%
    mutate(mod_time_vid = mod_time_to_tz_time(vid_file, timezone = timezone),
           vid_more_recent = ifelse(is.na(mod_time_vid), NA, mod_time_pngs > mod_time_vid))


  ## Get youtube IDs
  df$yt_md_ID = sapply(df$md_file,
                       function(fname) {
                         x = readLines(fname, warn = FALSE)
                         line <- grep(pattern = ("^!\\[.+\\]\\((?!\\.png)\\)|!\\[.+\\]\\(.+[^.png]\\)|^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)"),x,perl=TRUE)
                         ## get youtube ID
                         ## this will break if youtube ever decides
                         ## to change the length of their IDs
                         x = ifelse(!is.na(df$yt_md_link[df$md_file==fname]),
                                    str_sub(x[line],-12,-2),NA)
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
  return(L)
}
