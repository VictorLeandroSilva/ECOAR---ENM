
## memoria
rm(list = ls()) # Limpa memoria do ambiente 
memory.limit(1000000) # Limite de memoria para o script utilizar


## packages
library(spocc)
library(tidyverse)
library(rgbif)
library(taxize)


# check loaded packeges
search()

# directory
setwd("C:/Users/victo/OneDrive/Livros legais/ECOA-nicho-model")
getwd()
dir()

###---------------------------------------------------------------------------------------###

# species list

sp.enm <- c("Brotogeris chiriri")
sp.enm

#length(sp.enm)
# download data
i <-  1
for(i in 1:length(sp.enm)){
  
  #sp.syn = synonyms(sp.enm[i], db = 'itis')
  #sp.syn <- synonyms_df(sp.syn)
  
  re <- spocc::occ(query = "Brotogeris chiriri", from = c("gbif","vertnet"),
                 limit = 1000)
  da <- spocc::occ2df(re) 
  #transformando data em data.frame
  da <- spocc::occ2df(re) 
  da

  #transformando latitude e longitude em caracteres numericos
  da$latitude <- as.numeric(da$latitude)
  da$longitude <- as.numeric(da$longitude)
  readr::write_csv(da, paste0(sp.enm[i],".csv"))
  
  
}


###---------------------------------------------------------------------------------------###
