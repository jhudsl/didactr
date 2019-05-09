#' Translate Mansucript File
#'
#' @param file Manuscript markdown file
#' @param chunk Should the calls to Google Translate be chunked?
#' @param verbose Print diagnostic messages
#' @param fix_header should the header information be fixed,
#' such as `{ course-completeness: 100 }`.
#' @param target target language, see \code{\link{gl_translate}}
#' @param ... additional arguments to pass to
#'  \code{\link{gl_translate}}
#' @importFrom dplyr mutate select
#' @importFrom googleLanguageR gl_translate
#' @importFrom stats runif
#'
#' @return A `data.frame` of the text and translated text
#' @note The following changes of the translated text are
#' attempted:
#' Change `] (` without spaces for links
#' Keep links to be the link from original text
#' Replace asterisks and underscores for bolding
#' Keep back-ticked code as is
#' @export
#' @examples
#' auth_file = "~/Dropbox/Projects/cbds_translate/RClass-Translator.json"
#' if (file.exists(auth_file)) {
#' googleLanguageR::gl_auth(auth_file)
#' target = "es"
#' verbose = TRUE
#'  chunk = TRUE
#' file = system.file("extdata", "00_template.md", package = "didactr")
#' df = translate_manuscript(file)
#' }
translate_manuscript = function(
  file,
  target = "es",
  chunk = TRUE,
  fix_header = TRUE,
  verbose = TRUE,
  ...) {
  translatedText = NULL
  rm(list = "translatedText")

  check_manuscript_backticks(file)

  df = chunk_google_translate(
    file,
    target = target,
    chunk = chunk,
    fix_header = fix_header,
    ...)
  xdf = df

  df = xdf
  df = df %>%
    mutate(original_translated_text = translatedText)
  df = fix_image_links(df, verbose = verbose)
  df = fix_bolding(df, verbose = verbose)
  df = fix_at(df, verbose = verbose)
  df = fix_links(df, verbose = verbose)

  df = fix_back_ticks(df, verbose = verbose)
  df = fix_tags(df, verbose = verbose)
  df = fix_underscore(df, verbose = verbose)
  df = fix_exercises(df, verbose = verbose)

  return(df)
}


#' @export
#' @rdname translate_manuscript
translate_script = function(
  file,
  target = "es",
  chunk = TRUE,
  fix_header = FALSE,
  verbose = TRUE,
  ...) {
  translatedText = NULL
  rm(list = "translatedText")

  df = chunk_google_translate(
    file,
    target = target,
    chunk = chunk,
    fix_header = fix_header,
    ...)
  df = df %>%
    mutate(original_translated_text = translatedText)
  # df = fix_image_links(df, verbose = verbose)
  # df = fix_bolding(df, verbose = verbose)
  df = fix_at(df, verbose = verbose)
  df = fix_links(df, verbose = verbose)
  df = fix_back_ticks(df, verbose = verbose)
  # df = fix_tags(df, verbose = verbose)
  # df = fix_underscore(df, verbose = verbose)
  # df = fix_exercises(df, verbose = verbose)

  return(df)
}



#' @export
#' @rdname translate_manuscript
chunk_google_translate = function(file, chunk = TRUE,
                                  target = "es",
                                  fix_header = TRUE,
                                  ...) {
  txt = readLines(file, warn = FALSE)

  if (fix_header) {
    hdr = manuscript_header(txt)
  }

  # need to do this because of ` to '
  make_bad_string = function() {
    round(runif(1, min = 1e5, max = 1000000))
  }

  bad_string =  make_bad_string()
  bad_quote_string = make_bad_string()
  for (i in 1:10) {
    # just make another
    if (any(grepl(bad_string, txt))) {
      bad_string =  make_bad_string()
    }
    if (any(grepl(bad_quote_string, txt))) {
      bad_quote_string =  make_bad_string()
    }
  }

  nc = nchar(txt)
  original_df = flag_code_chunks(txt)

  original_df$nc = nc
  # original_df = data.frame(text = txt,
  #                          nc = nc,
  #                          stringsAsFactors = FALSE)

  # res = flag_code_chunks()
  # code = grep("^```", original_df$text)
  # if (length(code) > 0) {
  #   # has to have start/stop
  #   if (length(code) %% 2 != 0) {
  #     stop("Code blocks are not balanced (one open for one ending), stopping!")
  #   }
  #   code_start = code[seq(1, length(code) - 1, by = 2)]
  #   code_end = code[seq(2, length(code), by = 2)]
  #   code_indices = mapply(function(start, stop) {
  #     seq(start, stop)
  #   }, code_start, code_end, SIMPLIFY = FALSE)
  #   code_indices = c(unlist(code_indices))
  #   original_df$is_code = FALSE
  #   original_df$is_code[code_indices] = TRUE
  #   original_df$original_text = original_df$text
  #   original_df$text[ original_df$is_code ] = ""
  # }

  # single backticks
    # bad_string = paste(rep("Z", 10), collapse = "")
  if (any(grepl(bad_string, original_df$text))) {
    stop("need a different bad string")
  }
  original_df$text = gsub("`", bad_string, original_df$text)

  # distinguish where the quotes are supposed to go
  quote_string = paste0('" ', bad_quote_string)
  quote_string2 = sub(" ", "", quote_string)

  if (any(grepl(quote_string, original_df$text) |
          grepl(quote_string2, original_df$text))) {
    stop("need a different bad string for quotes")
  }
  original_df$text = gsub('" ', quote_string, original_df$text)


  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list = c("beginning_jump", "beginning_space", "extra_link_text",
              "has_tag", "is_image_link", "link_text", "link_value",
              "tag", "text", "translatedText"))

  if (chunk) {
    df = original_df
    df = df %>%
      mutate(item = floor(cumsum(nc) / 5000) + 1)

    items = unique(df$item)
    iitem = 1
    df$translatedText = NA
    for (iitem in items) {
      # get the chunk to run
      ind =  df$item == iitem &
        df$text != "" &
        # added for " "
        trimws(df$text) != ""
      run_txt = df$text[ ind ]
      # remove just ""
      if (sum(ind) != length(run_txt)) {
        print(paste("item is", iitem))
        stop("Something wrong with subsetting")
      }
      run_txt = paste(run_txt, collapse = "\n")
      res = googleLanguageR::gl_translate(
        run_txt, target = target,
        source = "en")
      ttext = res$translatedText
      ttext = strsplit(ttext, "\n")[[1]]
      if (sum(ind) != length(ttext)) {
        print(paste("item is", iitem))
        stop("Something wrong with output length!")
      }
      df$translatedText[ind] = ttext
    }
    no_trans = df$text[is.na(df$translatedText)]
    df = df %>%
      select(translatedText, text, everything())
    stopifnot(all(trimws(no_trans) == ""))
  } else {
    df = googleLanguageR::gl_translate(
      txt, target = target,
      source = "en")
    df = df %>%
      mutate(
        nc = nchar(text),
        item = floor(cumsum(nc) / 5000) + 1)
  }
  if (sum(original_df$is_code) > 0) {
    code_log_ind = original_df$is_code
    # replace the code back
    rep_txt = original_df$original_text[code_log_ind]
    df$translatedText[ code_log_ind] = rep_txt
    df$text[ code_log_ind] = rep_txt
    df$original_text[ code_log_ind] = rep_txt
  }

  if (any(original_df$is_image)) {
    code_log_ind = original_df$is_image
    # replace the code back
    df$translatedText[ code_log_ind ] =
      paste0("![", trimws(df$translatedText[ code_log_ind ]),
            "](", df$image_link[ code_log_ind ], ")")
  }

  df = df %>%
    mutate(
      text = gsub(bad_string, "`", text),
      translatedText = gsub(bad_string, "`", translatedText),

      # get quoting right
      text = gsub(paste0(" ", quote_string2), '" ', text),
      translatedText = gsub(paste0(" ", quote_string2), '" ', translatedText),

      # get quoting right
      text = gsub(paste0(" ", quote_string), '" ', text),
      translatedText = gsub(paste0(" ", quote_string), '" ', translatedText),


      text = gsub(quote_string2, '" ', text),
      translatedText = gsub(quote_string2, '" ', translatedText),

      # get quoting right
      text = gsub(quote_string, '" ', text),
      translatedText = gsub(quote_string, '" ', translatedText),

    )
  df = df %>%
    select(-nc)
  if (fix_header) {
    if (!is.null(hdr)) {
      ind = seq(hdr$start_ind, hdr$end_ind)
      df$translatedText[ind] = df$text[ind]
    }
  }
  df
}


#' @export
#' @rdname translate_manuscript
#' @param max_nchar number of characters to use for detection.
gl_detect_file = function(file, max_nchar = 2000) {
  text = NULL
  rm(list = "text")
  txt = readLines(file, warn = FALSE)
  nc = nchar(txt)
  df = data.frame(text = txt,
                  nc = nc,
                  stringsAsFactors = FALSE)
  df = df %>%
    filter(text != "")
  df = df %>%
    mutate(item = floor(cumsum(nc) / max_nchar) + 1)
  # get the chunk to run
  ind =  df$item == 1 & df$text != ""
  run_txt = df$text[ ind ]
  run_txt = paste(run_txt, collapse = "\n")
  res = googleLanguageR::gl_translate_detect(run_txt)
  res$text = NULL
  res = as.list(res)
  return(res)
}


replace_NA = function(df) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))

  df$translatedText = unlist(df$translatedText)
  df = df %>%
    mutate(translatedText =
             ifelse(is.na(translatedText) |
                      translatedText == "NA" |
                      text == "",
                    "",
                    translatedText)
    )
  df
}

fix_at = function(df, verbose = TRUE) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))


  if (verbose) {
    msg = "Fixing @"
    message(msg)
  }
  df = df %>%
    mutate(translatedText = gsub(" @ ", "@", translatedText))
  df = replace_NA(df)
  df
}


fix_tags = function(df, verbose = TRUE) {

  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))

  if (verbose) {
    msg = "Fixing Tags, such as {quiz}"
    message(msg)
  }
  front_tag = "^\\{.*\\}"
  df = df %>%
    mutate(
      has_tag = grepl(front_tag, text)
    )
  df = df %>%
    mutate(
      tag = ifelse(has_tag,
                   gsub(paste0("(", front_tag, ")", ".*"), "\\1", text),
                   "")
    )
  df = df %>%
    mutate(
      translatedText = gsub(front_tag, "", translatedText),
      translatedText = ifelse(has_tag,
                              paste0(tag, translatedText),
                              translatedText)
    ) %>%
    select(-has_tag, -tag)
  df = replace_NA(df)
  df
}

fix_underscore = function(df, verbose = TRUE) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))


  if (verbose) {
    msg = "Fixing Underscores"
    message(msg)
  }
  split_join = function(x, split = "_") {
    add_space = function(x) {
      ind = grepl("_$", x)
      x[ind] = paste0(x[ind], " ")
      x
    }
    x = add_space(x)
    ss = strsplit(x, split = split)
    ss = sapply(ss, function(r) {
      n = length(r)
      if (n > 1) {
        ind = unique(2:(n - 1))
        r[ind] = trimws(r[ind])
      }
      r = paste(r, collapse = "_")
      return(r)
    })
    ss
  }

  df = df %>%
    mutate(
      translatedText = split_join(translatedText)
    )
  df = replace_NA(df)
  df
}

# fix code
fix_back_ticks = function(df, verbose = TRUE) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))


  if (verbose) {
    msg = "Fixing Back Ticks"
    message(msg)
  }

  split_join = function(text, translatedText, split = "`") {
    # need to add " " for split
    add_space = function(x) {
      ind = grepl("`$", x)
      x[ind] = paste0(x[ind], " ")
      x
    }
    text = add_space(text)
    translatedText = add_space(translatedText)

    ss = strsplit(text, split = split)
    ss_trans = strsplit(translatedText, split = split)

    ss = mapply(function(translated, orig) {
      n = length(translated)
      if (n > 1) {
        if (n == 2) {
          msg = paste0("Something went wrong with the ",
                       "backticks (original then translated):")
          warning(msg)
          message(msg)
          message(orig)
          message(translated)
        } else {
          indices = seq(2, n - 1, by = 2)
          translated[indices] = orig[indices]
        }
      }
      translated = paste(translated, collapse = "`")
      return(translated)
    }, ss_trans, ss, SIMPLIFY = TRUE)


    ss
  }


  text = df$text
  translatedText = df$translatedText
  df = df %>%
    mutate(translatedText = split_join(text, translatedText))
  df = replace_NA(df)
  df
}



fix_bolding = function(df, verbose = TRUE) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))


  if (verbose) {
    msg = "Fixing Bolding"
    message(msg)
  }
  begin_ast_pat = "^(\\s*[*] )"
  df = df %>%
    mutate(
      beginning_jump = grepl(begin_ast_pat, text),
      beginning_space = ifelse(
        beginning_jump,
        sub(paste0(begin_ast_pat, ".*"), "\\1", text),
        "")
    )
  df = df %>%
    mutate(
      translatedText = ifelse(
        beginning_jump,
        sub(begin_ast_pat, "", translatedText),
        translatedText),
      translatedText = trimws(translatedText, which = "left")
    )
  xx = df

  split_join = function(x, split = "[*]") {
    add_space = function(x) {
      ind = grepl("[*]$", x)
      x[ind] = paste0(x[ind], " ")
      x
    }
    x = add_space(x)
    ss = strsplit(x, split = split)
    ss = sapply(ss, function(r) {
      n = length(r)
      if (n > 1) {
        ind = unique(2:(n - 1))
        r[ind] = trimws(r[ind])
      }
      r = paste(r, collapse = "*")
      return(r)
    })
    ss
  }
  df = xx
  df = df %>%
    mutate(
      translatedText = gsub(
        paste0("^([*]|)[*] "),
        "\\1*",
        translatedText),
      translatedText = gsub(
        paste0(" [*]([*]|)$"),
        "*\\1",
        translatedText)
    )
  df = df %>%
    mutate(
      translatedText = split_join(translatedText)
    )
  df = df %>%
    mutate(
      translatedText = paste0(
        beginning_space,
        translatedText)
    ) %>%
    select(-beginning_jump, -beginning_space)
  df = replace_NA(df)
  return(df)
}

fix_exercises = function(df, verbose = TRUE) {

  if (verbose) {
    msg = "Fixing Exercises/quizes"
    message(msg)
  }

  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))

  is_answer = answer = question_with_space = NULL
  rm(list = c("is_answer", "answer", "question_with_space"))

  is_question = is_regex = NULL
  rm(list = c("is_regex", "is_question"))
  itag = "exercises"

  tags = c("quiz", "exercise")
  for (itag in tags) {
    start_tag = paste0("^\\{", itag)
    end_tag = paste0("^\\{/", itag, "\\}")
    start_ind = grep(start_tag, df$text)
    end_ind = grep(end_tag, df$text)
    if (length(start_ind) != length(end_ind) |
        length(start_ind) > 1 | length(end_ind) > 1) {
      warning(paste0("trying to keep answers for ", itag,
                     " tags, but not same length for start/ind, skipping"))
      next
    }
    if (length(start_ind) > 0 & length(end_ind) > 0) {
      indices = seq(start_ind, end_ind)
      ddf = df[indices, ]
      ddf = ddf %>%
        mutate(is_answer = grepl("^([a-zA-Z])\\)", trimws(text)),
               answer = ifelse(is_answer,
                               sub("^([a-zA-Z])\\).*", "\\1",
                                   text),
                               "")
        )
      ddf = ddf %>%
        mutate(translatedText = ifelse(is_answer,
                                       sub("^.*\\)", ")", translatedText),
                                       translatedText),
               translatedText = ifelse(is_answer,
                                       paste0(answer, translatedText),
                                       translatedText)
        ) %>%
        select(-is_answer, -answer)
      # ? 3 in translated should turn into ?3 if that's how it is in text
      ddf = ddf %>%
        mutate(is_question = grepl("^\\s*[?]", text),
               question_with_space = grepl("^\\s*[?]\\s", text),
               translatedText =
                 ifelse(is_question & !question_with_space,
                        sub("[?]\\s*", "?", translatedText),
                        translatedText)
        )

      # keep regular expressions as is
      ddf = ddf %>%
        mutate(is_regex = grepl("^\\s*[!]\\s*/", text),
               translatedText =
                 ifelse(is_regex,
                        text,
                        translatedText)
        ) %>%
        select( -is_regex)

      ddf = ddf[, colnames(df)]
      df[indices, ] = ddf
    }
  } # end tag

  df = replace_NA(df)
  df
}


fix_links = function(df, verbose = TRUE) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))

  # split_join = function(
  #   x,
  #   split = "] (",
  #   collapser = "](",
  #   fixed = TRUE) {
  #   ss = strsplit(x, split = split, fixed = fixed)
  #   ss = sapply(ss, function(r) {
  #     r = paste(r, collapse = collapser)
  #     return(r)
  #   })
  #   ss
  # }

  split_join = function(translatedText, text) {
    ss_trans = strsplit(translatedText,
                        split = "] (", fixed = TRUE)
    ss = strsplit(text, split = "](", fixed = TRUE)

    ss = mapply(function(translated, orig) {
      n = length(translated)
      if (n > 1) {
        indices = seq(2, n)
        translated[indices] = sub("^.*\\)", "", translated[indices])
        orig[indices] = sub("^(.*\\)).*", "\\1", orig[indices])
        translated[indices] = paste0(orig[indices], translated[indices])
      }
      translated = paste(translated, collapse = "](")
      return(translated)
    }, ss_trans, ss, SIMPLIFY = TRUE)

    ss
  }

  if (verbose) {
    msg = "Fixing Links"
    message(msg)
  }
  df = df %>%
    mutate(
      translatedText = split_join(translatedText, text)
    )
  df = replace_NA(df)
  df
}

fix_image_links = function(df, verbose = TRUE) {
  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))


  if (verbose) {
    msg = "Fixing Image Links"
    message(msg)
  }

  link_pat_with_spaces = "!\\s*\\[(.*)\\]\\s*\\((.*)\\)(.*)"
  df = df %>%
    mutate(is_image_link = grepl(link_pat_with_spaces, x = text, perl = TRUE),
           link_value = ifelse(
             is_image_link,
             sub(text,
                 pattern = link_pat_with_spaces,
                 replacement = "\\2"),
             ""
           ),
           link_text = ifelse(
             is_image_link,
             sub(translatedText,
                 pattern = link_pat_with_spaces,
                 replacement = "\\1"),
             ""
           ),
           extra_link_text = ifelse(
             is_image_link,
             sub(translatedText,
                 pattern = link_pat_with_spaces,
                 replacement = "\\3"),
             ""
           )
    )
  df = df %>%
    mutate(translatedText = ifelse(
      is_image_link,
      paste0("![", link_text, "](", link_value, ")", extra_link_text),
      translatedText)) %>%
    select(-link_text, -link_value, -extra_link_text)
  df = replace_NA(df)
  df
}




manuscript_header = function(txt) {
  ttxt = trimws(txt)
  empty =  which(ttxt == "")[1]
  start_ind = grepl("^\\{", ttxt)
  end_ind = grepl("\\}$", ttxt)
  if (!any(start_ind) & !any(end_ind)) {
    return(NULL)
  }
  start_ind = which(start_ind)[1]
  end_ind = which(end_ind)[1]
  if (is.na(start_ind) | is.na(end_ind)) {
    stop(paste0("Error in parsing manuscript header, ",
                "start_ind: ", start_ind,
                ", end_ind: ", end_ind))
  }

  if (end_ind <= empty + 1) {
    hdr = txt[seq(start_ind, end_ind)]
    return(list(header = hdr,
                start_ind = start_ind,
                end_ind = end_ind))
  } else {
    return(NULL)
  }
}
