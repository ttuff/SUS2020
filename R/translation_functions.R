

# Basic list french translation -------------------------------------------


sus_translation_list <- function(to_translate){
  
  # translate name of lists
  names(to_translate) <-
    map_chr(names(to_translate), ~{translation_fr %>%
        filter(en == .x) %>%
        pull()})
  
  # reiterate in list depth to translate every names
  if (vec_depth(to_translate) > 2)
    to_translate <- map(to_translate, sus_translation_list)
  
  to_translate
}



# Reactive translation function for texts, lists and png ------------------


sus_translate <- function(to_translate) {
  # if it's in english
  if (sus_reactive_variables$active_language() == "en") {
    to_translate
    
  # if it's in french
  } else if (sus_reactive_variables$active_language() == "fr") {
    
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
