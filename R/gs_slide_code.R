
#' Get Slide Code from Google Slides
#'
#' @param id ID of Google Slides
#' @param open Should the `Rmd` be opened to an editor
#'
#' @return The output temporary_file
#' @export
#'
#' @importFrom rgoogleslides get_slides_properties
#' @importFrom utils file.edit
#'
#' @examples \dontrun{
#' if (check_didactr_auth) {
#' id = "1Tg-GTGnUPduOtZKYuMoelqUNZnUp3vvg_7TtpUPL7e8"
#' gs_slide_code(id, open = TRUE)
#' }
#' }
gs_slide_code = function(id, open = FALSE) {

  title = content = NULL
  rm(list = c("title", "content"))
  pp = rgoogleslides::get_slides_properties(id)


  ele = pp$slides$pageElements
  # names(ele) = seq_along(ele)
  shape = ele[[32]]$shape



  texts = grab_slide_text(pp)

  # i = 1
  code = lapply(texts, function(x) {
    # print(i)
    # i <<- i + 1
    if (is.null(x)) {
      return(NULL)
    }
    if (length(x) == 1 && is.na(x)) {
      return(NULL)
    }
    str = "#r(stats|)"
    x = x %>%
      filter(grepl(str, title) | grepl(str, content))
    x = x$content
    if (length(x) == 0) {
      x = NULL
    }
    x
  })

  ids = seq_along(code)
  tmp = lapply(ids, function(x) {
    the_code = code[[x]]
    the_code = c(paste0("\n```{r slide_number_", x, "}"),
                 the_code,
                 "```")
    code[[x]] <<- the_code
  })

  all_code = unlist(code)
  all_code = gsub("\v", "\n", all_code)
  all_code = strsplit(all_code, "\n")
  all_code = unlist(all_code)
  install_code = paste(
    "^",
    c("install.packages",
      "devtools::install_github",
      "install_github"),
    sep = "", collapse = "|")
  install_code = grepl(install_code, all_code)
  all_code[install_code] = paste0("#", all_code[ install_code])
  all_code = gsub("\u2018", "'", all_code)
  all_code = gsub("\u2019", "'", all_code)
  all_code = gsub("\u201c", '"', all_code)
  all_code = gsub("\u201d", '"', all_code)

  hdr = c("---",
          paste0('title: "Google Slide ID ', id, '"'),
          "output: html_document",
          "---")
  all_code = c(hdr, "", "", all_code)
  tfile = tempfile(fileext = ".Rmd")
  writeLines(all_code, tfile)

  if (open) {
    file.edit(tfile)
  }
  return(tfile)
}




grab_shape_text = function(shape) {
  st = shape$shapeType
  te = shape$text$textElements
  tc = sapply(te, function(r) {
    xx = r$textRun$content
    if (is.null(r)) {
      return("")
    }
    xx[is.na(xx)] = ""
    xx = paste(xx, collapse = "")

  })
  if (length(tc) == 0) {
    tc = NULL
  }
  if (is.null(tc) & is.null(st)) {
    return(NULL)
  }
  df = data_frame(
    shape_type = st,
    content = tc
  )
  # df = df %>%
  #   filter(!is.na(content))
  # df = df %>%
  #   filter(content != "")
  df
}

grab_slide_text = function(x) {
  xx = x$slides$pageElements
  texts = lapply(xx, function(r) {
    grab_shape_text(r$shape)
  })
  titles = lapply(xx, function(r) {
    r$title
  })
  texts = mapply(function(x, y) {
    if (is.null(y)) {
      y = NA
    }
    x$title = y
    x
  }, texts, titles, SIMPLIFY = FALSE)
  return(texts)
}
