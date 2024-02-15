### script download occurrences - spocc ###


## memory
rm(list = ls())

## packages
library(colorRamps)
library(mapr)
library(raster)
library(sf)
library(tidyverse)

# check loaded packeges
search()

###---------------------------------------------------------------------------------------###

## data
## occ
# directory
setwd("C:/Users/victo/OneDrive/Livros legais/ECOA-nicho-model")
dir()
# import data
occ <- readr::read_csv("Brotogeris chiriri.csv")
occ

## var
# directory
setwd("C:/Users/victo/OneDrive/Livros legais/ECOA-nicho-model/01_variables")
getwd()

# import data
var <- raster::raster("bio01.tif")
var

## limite (shape)
# directory
setwd("C:/Users/victo/OneDrive/Livros legais/ECOA-nicho-model/00_limit")
dir()
# import limit
lim <- sf::st_read("limit_brazil_longlat_wgs84.shp")
lim

### DOCUMENTAÇÃO

####################3
# map
ggplot() +
 geom_raster(data = raster::rasterToPoints(var) %>% tibble::as_tibble(), 
             aes(x, y, fill = layer )) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_sf(data = lim, color = "black", alpha = 0) +
  geom_point(data = occ, aes(x = longitude, y = latitude), alpha = .1) +
  theme_minimal()

###---------------------------------------------------------------------------------------###

## clear data

# 1. registers and date
occ.cl <- occ %>% 
  dplyr::distinct(longitude, latitude, .keep_all = TRUE) %>% # remove duplicates
  dplyr::filter(!is.na(longitude)) %>% # remove longitude with NAs
  dplyr::filter(!is.na(latitude)) %>% # remove latitude with NAs
  dplyr::filter(date > "1990-01-01") # filter by date
occ.cl

###---------------------------------------------------------------------------------------###

## 2. poins inside limit
# plot
ggplot() +
  geom_point(data = occ.cl %>% dplyr::select(longitude, latitude), aes(x = longitude, y = latitude), 
             color = c("black"), alpha = .3) +
  theme_minimal()


###---------------------------------------------------------------------------------------###

## 3. one point by cell

# raster original
var
plot(var, col = colorRamps::matlab.like2(100))

# copy
var.id <- var
plot(var.id, col = colorRamps::matlab.like(100))

# create a raster with ids from cells
var.id[] # values from cells

plot(is.na(var.id)) # na
plot(!is.na(var.id)) # not na

var.id[!is.na(var.id)] # values not na

ncell(var.id[!is.na(var.id)]) # cell numbers not na
seq(var.id[!is.na(var.id)]) # seq 1 to number of cells

var.id[!is.na(var.id)] <- seq(var.id[!is.na(var.id)])
plot(var.id, col = colorRamps::matlab.like2(100))

# extract values
occ.cl.lim.oppc <- occ.cl %>% 
  mutate(oppc = raster::extract(var.id, dplyr::select(occ.cl, longitude, latitude)))
occ.cl.lim.oppc

# verify
sort(table(occ.cl.lim.oppc$oppc))
barplot(sort(table(occ.cl.lim.oppc$oppc)))

# oppc
occ.oppc <- occ.cl.lim.oppc %>% 
  dplyr::distinct(oppc, .keep_all = TRUE) %>% 
  na.omit
occ.oppc

# verify
sort(table(occ.oppc$oppc))
barplot(sort(table(occ.oppc$oppc)))

# export
setwd("C:/Users/Victor Leandro/Documents/Modelagem_mestrado/Occ")
readr::write_csv(occ.oppc, "Brotogeris_chiriri_clean_lim_oppc.csv")
getwd()
###---------------------------------------------------------------------------------------###
