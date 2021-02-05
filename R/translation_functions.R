#### Translation functions #####################################################

# Basic list French translation -------------------------------------------

sus_translate_list <- function(x) {
  
  # translate name of lists
  names(x) <-
    map_chr(names(x), ~{
      if (is.null(.x)) NULL else {
        translation_fr %>%
        filter(en == .x) %>%
        pull()
      }})
  
  # Re-iterate in list depth to translate every name
  if (vec_depth(x) > 2) x <- map(x, ~{
    if (vec_depth(.x) > 1) sus_translate_list(.x) else (.x)
    })
  
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
      sus_translate_list(x)
    
    # png
    } else if (any(str_detect(x, "_en.png"))) {
      str_replace(x, "_en.png", "_fr.png")
      
    # Character
    } else if (is.character(x)) {
      
      # In some cases, there are multiple different strings to translate (e.g. 
      # m_dyk and the list created there). This loop will take care of it.
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
