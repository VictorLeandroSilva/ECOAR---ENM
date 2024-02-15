
## packages

library(colorRamps)
library(raster)
library(sf)
library(tidyverse)

# check loaded packeges
search()

# directory
setwd("C:/Users/victo/OneDrive/Livros legais/ECOA-nicho-model")
getwd()
dir()

###---------------------------------------------------------------------------------------###

## 1. limit
# directory
dir.create("00_limit")
setwd("00_limit")
getwd()

# download shape brasil
br <- raster::getData("GADM", country = "BRA", level = 0) %>% 
  sf::st_as_sf()
br

# plot
ggplot() +
  geom_sf(data = br) +
  theme_minimal()

# export limit
sf::st_write(br, "limit_brazil_longlat_wgs84.shp")

# erase file 
unlink("gadm36_BRA_0_sp.rds", force = TRUE)

# back one directory
setwd("..")
getwd()

###---------------------------------------------------------------------------------------###

## 2. climate variables
# directory
dir.create("01_variables")
setwd("01_variables")

# download - Pode baixar dados do futuro tamb?m 
#Presente
var <- raster::getData(name = "worldclim", var = "bio", res = 10) # res (resolu??o) vai est? em minutos
var
#Passado
var.pas <- raster::getData(name = "worldclim", var = "bio", res = 10) # res (resolu??o) vai est? em minutos
var.pas
#Futuro
var.fut <- raster::getData(name = "worldclim", var = "bio", res = 10) # res (resolu??o) vai est? em minutos
var.fut
# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
          aes(x, y, fill = bio1)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  theme_minimal()

# names -  mudar os nomes das variaveis para ordenar para manter a ordem 
names(var)
names(var) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(var)
var

## bioclimates
# BIO01 = Temperatura media anual
# BIO02 = Variacao da media diurna (media por mes (temp max - temp min))
# BIO03 = Isotermalidade (BIO02/BIO07) (* 100)
# BIO04 = Sazonalidade da temperatura (desvio padrao deviation *100)
# BIO05 = Temperatura maxima do mes mais quente
# BIO06 = Temperatura minima do mes mais frio
# BIO07 = Variacao da temperatura anual (BIO5-BIO6)
# BIO08 = Temperatura media do trimestre mais chuvoso
# BIO09 = Temperatura media do trimestre mais seco
# BIO10 = Temperatura media do trimestre mais quente
# BIO11 = Temperatura media do trimestre mais frio
# BIO12 = Precipitacao anual
# BIO13 = Precipitacao do mes mais chuvoso
# BIO14 = Precipitacao do mes mais seco
# BIO15 = Sazonalidade da precipitacao (coeficiente de variacao)
# BIO16 = Precipitacao do trimestre mais chuvoso
# BIO17 = Precipitacao do trimestre mais seco
# BIO18 = Precipitacao do trimestre mais quente
# BIO19 = Precipitacao do trimestre mais frio


###---------------------------------------------------------------------------------------###

## 3. adust extention of variables
# crop
setwd("00_limit")
br = shapefile("limit_brazil_longlat_wgs84.shp") # Se for usar um shape j? existente

##### Para Selecionar extensao #######
###Com as coordenadas 
#escolha no plot
plot(var[[1]])
drawExtent()
#insirir os limites 
#e<-extent(-117.6281,-26.5557,-59.48609, 26.83885)

# Se for usar os shapes, trocar os "e" por "br"
var.br <- raster::crop(var, br) %>% 
  raster::mask(br)
var.br

# plot
ggplot() +
  geom_raster(data = raster::rasterToPoints(var.br[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = bio01)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
  geom_sf(data = br, color = "black", fill = "white", alpha = 0) +
  theme_minimal()

plot(var.br)

data <- var.br

setwd("..")
dir()
setwd("01_variables")

# export
raster::writeRaster(x = data, filename = names(var.br), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

# erase folder
unlink("wc10", recursive = TRUE, force = TRUE)

###---------------------------------------------------------------------------------------###
