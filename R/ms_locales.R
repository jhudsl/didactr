#' Locales for Microsoft Translate
#'
#' @return A list of Locales and the Speech Voices
#' @export
#'
#' @examples
#' ms_locales()
ms_locales = function() {
  locales = jsonlite::fromJSON(
    '{
    "ar-EG": {"Female": "Microsoft Server Speech Text to Speech Voice (ar-EG, Hoda)"},
    "de-DE": {"Female": "Microsoft Server Speech Text to Speech Voice (de-DE, Hedda)",
    "Male": "Microsoft Server Speech Text to Speech Voice (de-DE, Stefan, Apollo)"},
    "en-AU": {"Female": "Microsoft Server Speech Text to Speech Voice (en-AU, Catherine)"},
    "en-CA": {"Female": "Microsoft Server Speech Text to Speech Voice (en-CA, Linda)"},
    "en-GB": {"Female": "Microsoft Server Speech Text to Speech Voice (en-GB, Susan, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (en-GB, George, Apollo)"},
    "en-IN": {"Male": "Microsoft Server Speech Text to Speech Voice (en-IN, Ravi, Apollo)"},
    "en-US": {"Female": "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
    "Male": "Microsoft Server Speech Text to Speech Voice (en-US, BenjaminRUS)"},
    "es-ES": {"Female": "Microsoft Server Speech Text to Speech Voice (es-ES, Laura, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (es-ES, Pablo, Apollo)"},
    "es-MX": {"Male": "Microsoft Server Speech Text to Speech Voice (es-MX, Raul, Apollo)"},
    "fr-CA": {"Female": "Microsoft Server Speech Text to Speech Voice (fr-CA, Caroline)"},
    "fr-FR": {"Female": "Microsoft Server Speech Text to Speech Voice (fr-FR, Julie, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (fr-FR, Paul, Apollo)"},
    "it-IT": {"Male": "Microsoft Server Speech Text to Speech Voice (it-IT, Cosimo, Apollo)"},
    "ja-JP": {"Female": "Microsoft Server Speech Text to Speech Voice (ja-JP, Ayumi, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (ja-JP, Ichiro, Apollo)"},
    "pt-BR": {"Male": "Microsoft Server Speech Text to Speech Voice (pt-BR, Daniel, Apollo)"},
    "ru-RU": {"Female": "Microsoft Server Speech Text to Speech Voice (pt-BR, Daniel, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (ru-RU, Pavel, Apollo)"},
    "zh-CN": {"Female": "Microsoft Server Speech Text to Speech Voice (zh-CN, HuihuiRUS)",
    "Female2": "Microsoft Server Speech Text to Speech Voice (zh-CN, Yaoyao, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (zh-CN, Kangkang, Apollo)"},
    "zh-HK": {"Female": "Microsoft Server Speech Text to Speech Voice (zh-HK, Tracy, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (zh-HK, Danny, Apollo)"},
    "zh-TW": {"Female": "Microsoft Server Speech Text to Speech Voice (zh-TW, Yating, Apollo)",
    "Male": "Microsoft Server Speech Text to Speech Voice (zh-TW, Zhiwei, Apollo)"}
}')

}

#' @rdname ms_locales
#' @export
ms_locale_df = function() {
  res = ms_locales()
  res = unlist(res)
  res = data.frame(locale = res, x = names(res),
                   stringsAsFactors = FALSE)
  rownames(res) = NULL
  ss = strsplit(res$x, split = "[.]")
  res$code = sapply(ss, function(x) x[1])
  res$Gender = sapply(ss, function(x) x[2])
  res$x = NULL
  df = structure(
    list(Code = c("ar-EG", "ca-ES", "da-DK", "de-DE", "en-AU",
                  "en-CA", "en-GB", "en-IN", "en-NZ", "en-US", "es-ES", "es-MX",
                  "fi-FI", "fr-CA", "fr-FR", "hi-IN", "it-IT", "ja-JP", "ko-KR",
                  "nb-NO", "nl-NL", "pl-PL", "pt-BR", "pt-PT", "ru-RU", "sv-SE",
                  "zh-CN", "zh-HK", "zh-TW"),
         Language = c("Arabic (Egypt), modern standard",
                      "Catalan (Spain)", "Danish (Denmark)", "German (Germany)", "English (Australia)",
                      "English (Canada)", "English (United Kingdom)", "English (India)",
                      "English (New Zealand)", "English (United States)", "Spanish (Spain)",
                      "Spanish (Mexico)", "Finnish (Finland)", "French (Canada)", "French (France)",
                      "Hindi (India)", "Italian (Italy)", "Japanese (Japan)", "Korean (Korea)",
                      "Norwegian (BokmÃ¥l) (Norway)", "Dutch (Netherlands)", "Polish (Poland)",
                      "Portuguese (Brazil)", "Portuguese (Portugal)", "Russian (Russia)",
                      "Swedish (Sweden)", "Chinese (Mandarin, simplified)", "Chinese (Hong Kong SAR)",
                      "Chinese (Mandarin, Taiwanese)")), .Names = c("code", "language"
                      ), row.names = c(NA, 29L), class = "data.frame")
  res = merge(res, df, by = "code", all.x = TRUE, sort = FALSE)
  return(res)
}

#' @rdname ms_locales
#' @export
ms_languages = function() {
  unique(ms_locale_df()$language)
}

#' @rdname ms_locales
#' @export
ms_language_codes = function() {
  unique(ms_locale_df()$code)
}

