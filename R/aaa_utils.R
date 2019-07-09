
# get script path and number of paragraphs
paragraph_from_script <- function(x) {
  if (file.exists(x)) {
    para = readLines(x, warn = FALSE)
    para = trimws(para)
    para = para[!para %in% c("", " ")]
    return(para)
  } else{
    return(NA)
  }
}

n_para = function(x) {
  x = paragraph_from_script(x)
  if (length(x) == 0) {
    return(0)
  }
  ifelse(all(is.na(x)), NA, length(x))
}

length0 = function(x) {
  length(x) == 0
}

length0_to_NA = function(x) {
  if (length0(x)) {
    x <- NA
  }
  x
}


drive_information = function(id,
                             timezone = "America/New_York",
                             ...) {
  name = gs_name = NULL
  rm(list = c("name", "gs_name"))

  authorized = check_didactr_auth(...)
  drive_info = googledrive::drive_get(id = id)

  if (nrow(drive_info) > 0) {
    drive_info = drive_info %>%
      rename(gs_name = name) %>%
      mutate(course_info = gs_name)

    mod_time_gs = sapply(drive_info$drive_resource,
                         function(x) {
                           x$modifiedTime
                         })
    drive_info$mod_time_gs = lubridate::ymd_hms(
      mod_time_gs)
    drive_info$mod_time_gs = lubridate::with_tz(
      drive_info$mod_time_gs,
      tzone = timezone)
    drive_info$drive_resource = NULL
  } else {
    return(NULL)
  }
  return(drive_info)
}


drive_find_folder = function(
  pattern = "^cds",
  ..., shared_with_me = FALSE) {

  args = list(...)
  args$pattern = pattern
  if (shared_with_me) {
    if ("q" %in% names(args)) {
      warning("q is overridden when finding")
    }
    args$q = "sharedWithMe"
  }
  args$type = "folder"
  do.call(googledrive::drive_find, args = args)
}


#' Google Slides Helper Functions
#'
#' @param file markdown file for manuscript
#'
#' @return A scalar character vector
#' @export
#' @rdname gs_helpers
gs_id_from_slide = function(file) {
  if (!file.exists(file)) {
    return(NA_character_)
  }
  x = readLines(file, warn = FALSE)
  ind = grep(x, pattern = "\\[(S|s)lides\\]")
  if (length(ind) > 0) {
    x = x[ind]
  } else {
    x = x[grep(x, pattern = ".*presentation/d/")]

  }
  if (!any(grepl("http", x))) {
    return(NA_character_)
  }
  x = sub(".*\\(\\s*(http.*)\\s*\\).*", "\\1", x)
  x = unlist(sapply(x, function(r) httr::parse_url(r)$path))
  x = sub("/edit$", "", x)
  x = sub("/export/.*", "", x)
  x = basename(x)
  x = stats::na.omit(x)
  x = x[ nchar(x) > 5 ]
  ux = unique(x)
  if (length(ux) > 1) {
    warning(paste0("Multiple sheets identified! Taking most frequent.",
                   "  Please check ",
                   file))
    x = sort(table(x), decreasing = TRUE)
    x = names(x)[1]
    # x = x[1]
  } else {
    x = ux
  }
  if (length(x) == 0 || grepl("\\(\\)", x)) {
    return(NA_character_)
  }
  if (nchar(x) < 10) {
    warning(paste0("ID extracted is ", x, ", seems short"))
  }
  return(x)
}

######################################
# this returns the actual links in the text
######################################
#' @export
#' @rdname gs_helpers
get_image_link_from_slide = function(file) {
  x = readLines(file, warn = FALSE)
  x = grep(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", value = TRUE)
  x = sub(x, pattern = "!\\[(.*)\\]\\(((resources/|)images.*)\\)", replacement = "\\1")
  # if (length(x) == 0) {
  #   return(NA)
  # }
  return(x)
}

######################################
# this returns the actual image filenames referenced
# we will check to see if all images referenced exist
######################################
#' @export
#' @rdname gs_helpers
get_image_from_slide = function(file) {
  x = readLines(file, warn = FALSE)
  x = grep(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", value = TRUE)
  x = sub(x, pattern = "!\\[.*\\]\\(((resources/|)images.*)\\)", replacement = "\\1")
  # if (length(x) == 0) {
  #   return(NA)
  # }
  return(x)
}


list_one_file = function(x, ending = "pdf") {
  pdfs = list.files(
    pattern = paste0("[.]", ending),
    path = x,
    full.names = TRUE)
  if (length(pdfs) > 1) {
    warning(paste0(x, " had more than one ", ending, "! ",
                   "Only grabbing first"))
    pdfs = pdfs[1]
  }
  pdfs = length0_to_NA(pdfs)
  return(pdfs)
}



mod_time_to_tz_time = function(x, timezone) {
  mod_times = file.info(x)$mtime
  mod_times = lubridate::ymd_hms(mod_times, tz = Sys.timezone())
  mod_times = lubridate::with_tz(mod_times, tzone = timezone)
  return(mod_times)
}





png_pattern = function() {
  paste0("^!\\[.+\\]\\((?!\\.png)\\)|",
         "^!\\[\\]\\((?!\\.png)\\)|",
         "^!\\[.+\\]\\((?!\\.png)\\)|",
         "!\\[.+\\]\\(.+[^.png]\\)|",
         "^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)")
}

yt_pattern = function() {
  paste0("^!\\[.+\\]\\(https\\:\\/\\/www\\.youtu.+\\)|",
         "^!\\[.+\\]\\(https\\:\\/\\/youtu.+\\)")
}

is.Token = function(token) {
  inherits(token, "Token")
}


na_false = function(test) {
  test[ is.na(test)] = FALSE
  test
}

na_true = function(test) {
  test[ is.na(test)] = TRUE
  test
}

add_gh_collaborator = function(owner, repo,
                               collaborator = "leanpub",
                               auth_token) {
  lapply(collaborator, function(collab) {
    gh::gh(paste0(
      "PUT /repos/", owner, "/",
      repo, "/collaborators/",
      collab), .token = auth_token)
  })
  res = gh::gh(paste0(
    "GET /repos/", owner, "/",
    repo, "/collaborators"), .token = auth_token)
  collabs = sapply(res, function(x) {
    x$login
  })
  result = collaborator %in% collabs
  names(result) = collaborator
  if (!all(result)) {
    warning("Not all collaborators have been added!")
  }
  result
}


os_type <- function() {
  .Platform$OS.type
}

sys_type <- function() {
  if (os_type() == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (os_type() == "unix") {
    # "unix"
    "linux"
  } else {
    stop("Unknown OS")
  }
}


png_url = function(id, page_id) {
  paste0(
    "https://docs.google.com/presentation/d/",
    id, "/export/png?id=", id,
    "&pageid=", page_id)
}

download_png_urls = function(urls) {
  res = vapply(urls, function(url) {
    tfile = tempfile(fileext = ".png")
    httr::GET(url, httr::write_disk(tfile))
    tfile
  }, FUN.VALUE = character(1))
  return(res)
}
