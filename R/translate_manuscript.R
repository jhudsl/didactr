

#' Translate Mansucript File
#'
#' @param file Manuscript markdown file
#' @param chunk Should the calls to Google Translate be chunked?
#' @param verbose Print diagnostic messages
#' @param target target language, see \code{\link{gl_translate}}
#' @param ... additional arguments to pass to
#'  \code{\link{gl_translate}}
#' @importFrom dplyr mutate select
#' @importFrom googleLanguageR gl_translate
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
  verbose = TRUE,
  ...) {
  df = chunk_google_translate(
    file,
    target = target,
    chunk = chunk,
    ...)
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
  verbose = TRUE,
  ...) {
  df = chunk_google_translate(
    file,
    target = target,
    chunk = chunk,
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
                                  ...) {
  txt = readLines(file)


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

  # bad_string = paste(rep("Z", 10), collapse = "")
  if (any(grepl(bad_string, txt))) {
    stop("need a different bad string")
  }
  txt = gsub("`", bad_string, txt)

  # distinguish where the quotes are supposed to go
  quote_string = paste0('" ', bad_quote_string)
  quote_string2 = sub(" ", "", quote_string)

  if (any(grepl(quote_string, txt) |
          grepl(quote_string2, txt))) {
    stop("need a different bad string for quotes")
  }
  txt = gsub('" ', quote_string, txt)


  beginning_jump = beginning_space = extra_link_text = has_tag = NULL
  is_image_link = link_text = link_value = tag = NULL
  text = translatedText = NULL
  rm(list=c("beginning_jump", "beginning_space", "extra_link_text",
            "has_tag", "is_image_link", "link_text", "link_value",
            "tag", "text", "translatedText"))

  if (chunk) {
    nc = nchar(txt)
    df = data.frame(text = txt,
                    nc = nc,
                    stringsAsFactors = FALSE)
    df = df %>%
      mutate(item = floor(cumsum(nc) / 5000) + 1)

    items = unique(df$item)
    iitem = 1
    df$translatedText = NA
    for (iitem in items) {
      # get the chunk to run
      ind =  df$item == iitem & df$text != ""
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
    stopifnot(all(no_trans == ""))
  } else {
    df = googleLanguageR::gl_translate(
      txt, target = target,
      source = "en")
    df = df %>%
      mutate(
        nc = nchar(text),
        item = floor(cumsum(nc) / 5000) + 1)
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
  df
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
                       "backticks (orig then trans):")
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

  tags = c("quiz", "exercise", "exercises")
  itag = "exercises"

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
        mutate(translatedText = sub("^.*\\)", ")", translatedText),
               translatedText = paste0(answer, translatedText)
        ) %>%
        select(-is_answer, -answer)
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



