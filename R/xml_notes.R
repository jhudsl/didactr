#' Get Notes from XML
#'
#' @param file XML file from a PPTX
#'
#' @return A character vector
#' @export
#'
#' @importFrom dplyr %>%
xml_notes = function(file) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("xml2 package required to use xml_notes")
  }
  xdoc = xml2::read_xml(file)
  txt = xdoc %>% xml2::xml_find_all("//a:t") %>% xml2::xml_text()
  txt = paste(txt, collapse = " ")
}

