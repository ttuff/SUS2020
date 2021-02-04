#### Translation functions #####################################################

# Basic list french translation -------------------------------------------

sus_translation_list <- function(x) {
  
  # translate name of lists
  names(x) <-
    map_chr(names(x), ~{translation_fr %>%
        filter(en == .x) %>%
        pull()
      })
  
  # Re-iterate in list depth to translate every name
  if (vec_depth(x) > 2) x <- map(x, sus_translation_list)
  
  x
}


# Reactive translation function for text, lists and png -------------------

sus_translate <- function(x) {
  # English
  if (sus_reactive_variables$active_language() == "en") {
    x
    
  # French
  } else if (sus_reactive_variables$active_language() == "fr") {
    
    # List
    if (is.list(x)) {
      sus_translation_list(x)
    
    # png
    } else if (any(str_detect(x, "_en.png"))) {
      str_replace(x, "_en.png", "_fr.png")
      
    # Character
    } else if (is.character(x)) {
      
      # In some cases, there are multiple different strings to translate (ex. did
      # you know and the list created there). This loop will take care of it.
      translated <- vector("character", length(x))
      
      for (i in 1:length(x)) {
        translated[i] <- 
          translation_fr %>%
          filter(en == x[[i]]) %>%
          pull()
      }
      
      translated
  }
  }
}
