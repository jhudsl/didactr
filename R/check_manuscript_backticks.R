#' @rdname translate_manuscript
#' @export
check_manuscript_backticks = function(file) {
  file = readLines(file)
  bad = grepl("\\s+```", file)
  if (any(bad)) {
    stop(paste0("Bad backticks on lines",
                paste(which(bad), collapse = ","))
    )
  }
}
