#' Translate Swirl YAML to another language
#'
#' @param file YAML file for Swirl lesson
#' @param outfile output YAML file for translated Swirl lesson
#' @param detect Should \code{\link{gl_detect_file}} be run to
#' check that the language is not currently the target?
#' Must be google Language authorized using \code{\link{gl_auth}}
#' @param target language to translate to, see \code{\link{gl_translate}}
#' @param verbose Print diagnostic messages
#'
#' @return A list of results from \code{\link{commit_to_slides}}
#' @export
#'
#' @importFrom rgoogleslides add_delete_text_request add_insert_text_request
#' @importFrom googledrive is_dribble
#'
#' @importFrom yaml yaml.load_file write_yaml
#' @examples
#' \dontrun{
#' x = c("- Class: meta",
#' "  Course: Test Class", "  Lesson: Test lesson",
#' "  Author: Shannon Ellis", "", "- Class: text",
#' "  Output: This is an example with code `df`.")
#' tfile = tempfile(fileext = ".yml")
#' writeLines(x, tfile)
#' translate_swirl(tfile, outfile = tfile)
#' readLines(tfile)
#' }
translate_swirl = function(
  file,
  outfile = tempfile(fileext = ".yml"),
  target = "es",
  detect = TRUE,
  verbose = TRUE) {

  yaml = yaml::yaml.load_file(file)

  # only options are
  # c("Class", "Output", "CorrectAnswer", "AnswerTests", "Hint",
  # "AnswerChoices")

  hints = lapply(yaml, `[[`, "Hint")
  output = lapply(yaml, `[[`, "Output")

  null_empty = function(x) {
    if (is.null(x)) {
      x = ""
    }
    x
  }

  new_hints = sapply(hints, null_empty)
  new_out = sapply(output, null_empty)

  if (!is_language_auth()) {
    stop("Google Language is not Authorized, see gl_auth")
  }
  # Get all slide text

  make_bad_string = function() {
    x = round(runif(1, min = 1e5, max = 1000000))
    x = paste0(x, ";")
  }

  tb_df = dplyr::bind_rows(
    data_frame(
      text_content = new_hints,
      type = "Hint"
    ),
    data_frame(
      text_content = new_out,
      type = "Output"
    )
  )
  if (nrow(tb_df) > 0) {
    bad_string =  make_bad_string()
    for (i in 1:10) {
      # just make another
      if (any(grepl(bad_string, tb_df$text_content))) {
        bad_string =  make_bad_string()
      }
    }
    stopifnot(!any(grepl(bad_string, tb_df$text_content)))
    tb_df$text_content = gsub("\n", bad_string,
                              tb_df$text_content)

    tb = tb_df$text_content
    file = tempfile()
    writeLines(tb, con = file)
    if (verbose) {
      message("Temporary File Created: ", file,
              " with bad_string: ", bad_string)
    }
    if (detect) {
      if (verbose) {
        message("Detecting Language")
      }
      out = gl_detect_file(file)
      if (out$language == target) {
        message(page_id, " already in target language")
        return(NULL);
      }
    }

    df = chunk_google_translate(
      file,
      target = target,
      chunk = TRUE,
      fix_header = FALSE)
    df$translatedText = gsub(
      bad_string, "\n",
      df$translatedText)
    df$text = gsub(
      bad_string, "\n",
      df$text)
    tb_df$text_content = gsub(
      bad_string, "\n", tb_df$text_content)
    tb = gsub(bad_string, "\n", tb)

    tb_new = df$translatedText
    stopifnot(length(tb) == length(tb_new))
    stopifnot(!any(grepl(bad_string, tb_new)))
    stopifnot(!any(grepl(bad_string, tb)))

    df$type = tb_df$type
    df$translatedText[ is.na(df$translatedText)] = ""
    new_data = split(df, df$type)

    i = 1
    out_yaml = yaml
    for (i in seq_along(yaml)) {
      x = out_yaml[[i]]
      if ("Hint" %in% names(x) &
          !is.null(x$Hint)) {
        x$Hint = new_data$Hint$translatedText[i]
      }
      if ("Output" %in% names(x) &
          !is.null(x$Output)) {
        x$Output = new_data$Output$translatedText[i]
      }
      out_yaml[[i]] = x
    }

    yaml::write_yaml(x = out_yaml, file = outfile)
    return(outfile)
  } else {
    if (file != outfile) {
      file.copy(file, outfile, overwrite = TRUE)
    }
  }
  attr(outfile, "target_language") = target
  return(outfile)
}
