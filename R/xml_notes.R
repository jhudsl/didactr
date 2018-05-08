#' Get Notes from XML
#'
#' @param file XML file from a PPTX
#'
#' @return A character vector
#' @export
#'
#' @importFrom xml2 read_xml xml_text xml_find_all
#' @importFrom dplyr %>%
xml_notes = function(file) {
  xdoc = read_xml(file)
  txt = xdoc %>% xml_find_all("//a:t") %>% xml_text()
  txt = paste(txt, collapse = " ")
}

