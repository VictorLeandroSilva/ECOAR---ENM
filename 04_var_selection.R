### script variables download and adjust extention - worldclim ###
## Victor Leandro Silva
## Script adaptado de thadeu sobral (mauricio vancine)
##
# 05/05/2019

# memory
rm(list = ls())

# packages
library(caret)
library(colorRamps)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(psych)
library(RStoolbox)
library(raster)
library(tidyverse)
library(usdm)
library(viridis)

# check loaded packeges
search()

# directory
setwd("C:/Users/victo/OneDrive/Livros legais/ECOA-nicho-model/01_variables")
getwd()
dir()

###---------------------------------------------------------------------------------------###

# list variables
ti <- dir(pattern = ".tif$")
ti

# import rasters
var <- raster::stack(ti)
var
plot(var[[1:4]], col = colorRamps::matlab.like2(100))

# extract values
var.da <- var %>% 
  raster::values() %>% 
  na.omit

# verify
head(var.da)
dim(var.da)

###---------------------------------------------------------------------------------------###

## variable selection
# back one directory
setwd("..")

# directory
dir.create("02_selection")
setwd("02_selection")

## 1.correlation
# create directory
dir.create("01_correlation") 
setwd("01_correlation")

# correlation
corr <- cor(var.da, method = "spearman")
corr

# export
readr::write_csv(tibble::as_tibble(corr), "correlation.csv")

# correlation plot
corrplot::corrplot(corr, type = "lower", diag = FALSE, tl.srt = 45, mar = c(3, 0.5, 2, 1))

# export figure
png("corr.tif", w = 18, he = 18, units = "cm", res = 300)
corrplot::corrplot(corr, type = "lower", diag = FALSE, tl.srt = 45, mar = c(3, 0.5, 2, 1))
dev.off()


## select variables
# verify
caret::findCorrelation(corr, cutoff = .7, names = TRUE, verbose = TRUE)

# correlated variables
fi <- caret::findCorrelation(corr, cutoff = .7)
fi

# new test
corr2 <- cor(var.da[, -fi], method = "spearman")
corr2

# verify
caret::findCorrelation(corr2, cutoff = .7, names = TRUE, verbose = TRUE)

# export
readr::write_csv(tibble::as_tibble(corr2), "correlation2.csv")

# correlation plot
corrplot::corrplot(corr2, type = "lower", diag = FALSE, tl.srt = 45, mar = c(3, 0.5, 2, 1))

# export figure
png("corr2.tif", w = 20, he = 15, units = "cm", res = 300)
corrplot::corrplot(corr2, type = "lower", diag = FALSE, tl.srt = 45, mar = c(3, 0.5, 2, 1))
dev.off()

###---------------------------------------------------------------------------------###

# 2. vif
# back one directory
setwd("..")

# create directory
dir.create("02_vif") 
setwd("02_vif") 

# vif 10
vi.10 <- usdm::vifstep(var.da, th = 10, maxobservations = nrow(var.da))
vi.10
vi.10@results

# vif 05
vi.05 <- usdm::vifstep(var.da, th = 5, maxobservations = nrow(var.da))
vi.05
vi.05@results

# export
readr::write_csv(vi.10@results, "vif_10.csv")
readr::write_csv(vi.05@results, "vif_05.csv")

###---------------------------------------------------------------------------------###

# 3. factorial analysis

# Precisa escolher a variavel bruta na hora de modelar

# back one directory
setwd("..")

# create directory
dir.create("03_factorial") 
setwd("03_factorial") 

# preliminaries analysis
# kmo e bartlett
KMO(cor(var.da)) # > 0.5
cortest.bartlett(cor(var.da), n = nrow(var.da)) # p < 0.05

# screeplot - number os axis
psych::fa.parallel(var.da, fa = "fa")

# exportar screeplot
png("screeplot_fatorial.tif", wi = 20, he = 15, un = "cm", res = 300)
psych::fa.parallel(var.da, fa = "fa") 
dev.off()

# fatorial
fa.6 <- psych::fa(var.da, nfactors = 7, rotate = "varimax")
fa.6

# loadings - Copiar o SS loading na tabela escrita no final
# ela n?o escreve na tabela final 

fa.6$loadings

# export
# Seleciona as variaveis na tabela com o valor mais alto de rela??o com os eixos
# Precisa selecionar as variaveis de acordo com a biologia da esp?cie

write.csv(fa.6$loadings %>% abs %>% round(2), "fa_6_loadings.csv")

###---------------------------------------------------------------------------------###

# 4. principal component analysis
# back one directory
setwd("..")

# create directory
dir.create("04_pca") 
setwd("04_pca") 

# pca
pca <- FactoMineR::PCA(var.da, scale.unit = TRUE, graph = FALSE)
pca

# eigenvalues
factoextra::get_eig(pca) %>% 
  round(2)

# eigenvalues plot
factoextra::fviz_eig(pca, addlabels = TRUE, ylim = c(0, 51), ggtheme = theme_classic())
ggsave("00_screeplot.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# contributions
pca$var$contrib

# selection
pca$var$contrib %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(var = rownames(pca$var$contrib)) %>%
  dplyr::select(var, Dim.1, Dim.2) %>%
  dplyr::arrange(desc(Dim.1))

# contributions
factoextra::fviz_contrib(pca, choice = "var", axes = 1, ggtheme = theme_classic())
ggsave("01_contributions_pc1.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

factoextra::fviz_contrib(pca, choice = "var", axes = 2, ggtheme = theme_classic())
ggsave("02_contributions_pc2.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

# biplot
factoextra::fviz_pca(pca, geom = "point", col.ind = "black", alpha.ind = .05)
ggsave("03_biplot_pca.tiff", he = 15, wi = 20, un = "cm", dpi = 300)

###----------------------------------------------------------------------------------###

## pca of raster
pca.ra <- RStoolbox::rasterPCA(var, spca = TRUE) 
pca.ra

# contribution of each axis 
summary(pca.ra$model)
pca$eig

# plot pf pcas as variables
ggplot() +
  geom_raster(data = raster::rasterToPoints(pca.ra$map[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = PC1)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  theme_minimal()

# export
raster::writeRaster(x = pca.ra$map[[1:6]], filename = paste0("pc0", 1:6), bylayer = TRUE, 
                    options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)

###---------------------------------------------------------------------------------###
