
# Function titletextSus
# Ty Tuff

# This function adds a standard text box 
# with the modules Title and text displayed 
# within an expanding and contracting window.
library(spocc)
library('taxize')

speciesOccDataSus_UI <- function(id,i18n){ 
  
  ns <- NS(id)
  
  tagList(
  
    
    
    )
  }



speciesOccDataSus_Server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 spp <- c('Danaus plexippus','Accipiter striatus','Pinus contorta')
                 dat <- occ(query=spp, from='gbif', gbifopts=list(hasCoordinate=TRUE))
                 dat <- fixnames(dat)
                 dat <- occ2df(dat)
                
                 
               })
}






