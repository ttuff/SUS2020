library(rvest)
library(tidyverse)

common_names <- read.csv("translations/Common_names.csv")

common_names <- 
  common_names %>% 
  as.tibble() %>% 
  mutate(verbatimScientificName = str_replace_all(verbatimScientificName, " ", "_"), 
         verbatimScientificName = str_remove_all(verbatimScientificName, "_L.|Ã—_|R.Br."),
         fr_common_names = as.character(fr_common_names))



for(i in 1:nrow(common_names)) {
  for(a in 1) {
    try({
  common_names[i,3] <- 
    read_html(paste0("https://fr.wikipedia.org/wiki/", pull(common_names[i,1]))) %>% 
    as.character() %>% 
    str_extract(pattern = '<title>.*</title>') %>%
    str_match(pattern = ">.*<") %>% 
    str_remove_all(pattern = c(">| — Wikipédia<"))
  break
    })
  }
  print(i)
}


common_names <- 
common_names %>% 
  mutate(verbatimScientificName = str_replace_all(verbatimScientificName, "_", " ")) %>% 
  mutate(fr_common_names = ifelse(is.na(fr_common_names), verbatimScientificName, fr_common_names))

write.csv(common_names, file = "translations/Common_names_fr.csv")
