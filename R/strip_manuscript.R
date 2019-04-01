#' Strip Markdown from Manuscript
#'
#' @param file A markdown file for parsing
#'
#' @return A \code{data.frame} with text and edits
#' @export
#'
#' @importFrom english as.english
#' @examples
#' library(dplyr)
#'  library(english)
#'  file = system.file("extdata", "08_DST_R_packages.md",
#'  package = "didactr")
#'  # from https://stackoverflow.com/questions/37462126/regex-match-markdown-link
#'  md_regex = "(?:__|[*#])|\\[(.*?)\\]\\(.*?\\)"
#'  res = strip_manuscript(file)
strip_manuscript = function(file) {
  header = is_code = item_number = item_list = NULL
  text = image = bold_header = NULL
  rm(list = c("is_code", "item_list", "item_number",
              "header", "bold_header", "text", "image"))
  x = readLines(file, warn = FALSE)
  df = flag_code_chunks(x = x)
  # remove images
  df = df %>%
    mutate(
      text = trimws(text),
      empty = text == "",
      header = grepl("^#", text) & !is_code,
      bold_header = grepl("^[*].*[*]$", text),
      image = grepl("^!\\[.*\\)$", text),
      bullets = grepl("^\\s*[*] ", text))
  df = df %>%
    mutate(
      item_list = grepl("^\\s*\\d*([.]|\\)) ", text),
      item_number = ifelse(item_list,
                           sub("^\\s*(\\d*)([.]|\\)).*", "\\1", text),
                           ""),
      item_number = as.numeric(item_number),
      item_number = english::as.english(item_number),
      item_number = as.character(item_number),
      text = ifelse(item_list,
                    sub("^\\s*(\\d*)([.]|\\))(.*)", "\\2\\3", text),
                    text),
      text = ifelse(item_list,
                    paste0(item_number, text),
                    text)

    )
  df$text[ !df$header & !df$is_code & !df$empty & !df$image & !df$bold_header]
  df$text[ df$item_list]
  df = df %>%
    mutate(text = ifelse(header, "", text),
           text = ifelse(bold_header, "", text),
           text = ifelse(image, "", text),
           text = ifelse(is_code, "", text)
    )


  return(df)
}

