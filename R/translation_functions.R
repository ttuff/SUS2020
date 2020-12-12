### Functions to use when i18n can't be used, ##################################
### for example in the translation of a list. ##################################

# sus_translate_string <- function(string, keep_english_if_no_translation = FALSE) {
#   
#   # By default, send an error if string is not in the CSV. Override with second
#   # argument = T.
#   if(keep_english_if_no_translation == F) {
#     if(!(string %in% rownames(i18n$get_translations())))
#       stop("No translation in CSV, use `keep_english_if_no_translation = T` to override")
#   }
#   
#   # Take out the HTML and keep the translated string only
#   str_remove_all(
#     str_match(as.character(i18n$t(string)), ">.*<"),
#     ">|<")
# 
# }
# 
# 
# sus_translate_list_names <- function(list_to_translate, keep_english_if_no_translation = FALSE) {
# 
#   # translate name of lists
#   names(list_to_translate) <- 
#     map_chr(names(list_to_translate), 
#             sus_translate_string, keep_english_if_no_translation)
#   
#   # reiterate in list depth to translate every names
#   if (vec_depth(list_to_translate) > 1) 
#     list_to_translate <- map(list_to_translate, 
#                              sus_translate_list_names, keep_english_if_no_translation)
#   
#   list_to_translate
# }


sus_translation_list <- function(to_translate){
  
  # translate name of lists
  names(to_translate) <-
    map_chr(names(to_translate), ~{translation_fr %>%
        filter(en == .x) %>%
        pull()})
  
  # reiterate in list depth to translate every names
  if (vec_depth(to_translate) > 1)
    to_translate <- map(to_translate, sus_translation_list)
  
  to_translate
}

sus_translate <- function(to_translate) {
  # if it's in english
  if (r$active_language() == "en") {
    to_translate
    
  # if it's in french
  } else if (r$active_language() == "fr") {
    
    # if it's a list
    if (is.list(to_translate)) {
      sus_translation_list(to_translate)
    
    # if it's a png
    } else if (str_detect(to_translate, "_en.png") == T) {
      str_replace(to_translate, "_en.png", "_fr.png")
      
    #if it's a character
    } else if ((is.character(to_translate)) == T) {
      translation_fr %>%
        filter(en == to_translate) %>%
        pull()
  }
  }
}
