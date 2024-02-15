### script enm dismo - multiple algorithms ###

# mauricio vancine
# 12-12-2018

# memory
rm(list = ls())

# packages
library(colorRamps)
library(dismo)
library(kernlab)
library(randomForest)
library(raster)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_201") 
library(rJava)
library(tidyverse)
library(viridis)

###---------------------------------------------------------------------------###

## import data
# 1. occ
# directory
setwd(choose.dir())
dir()

# occurrences
occ <- readr::read_csv("occ_Tropidurus_cocorobensis_clean_lim_oppc.csv") %>% 
  dplyr::filter(name =="Tropidurus cocorobensis")
occ

# create shapefile
occ.sh <-  occ %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ.sh

# plot
ggplot() + 
  geom_sf(data = occ.sh, col = "black", size = 2.5, alpha = .4) +
  theme_minimal()

# export shapefile
sf::st_write(occ.sh, "occ_Elaenia_oppc.shp")

###---------------------------------------------------------------------------###

##  2. variables
# directory
setwd(choose.dir())
dir()

# list files
ti <- dir(patt = ".tif$")
ti

# import rasters
var <- raster::stack(ti)
var

# var do futuro

#cc45bi50
setwd(choose.dir())
dir()
ti <- dir(patt = ".tif$")
ti
var.cc45bi50 <- raster::stack(ti)

#cc60bi50
setwd(choose.dir())
dir()
ti <- dir(patt = ".tif$")
ti
var.cc60bi50 <- raster::stack(ti)

#cc84bi50
setwd(choose.dir())
dir()
ti <- dir(patt = ".tif$")
ti
var.cc85bi50 <- raster::stack(ti)

# plot
plot(var, col = viridis::viridis(100))

# Se for usar PCA para as variaveis, trocar o campo "fill = bio02" pelo nome do PC
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = occ, aes(lon, lat), size = 2.5, alpha = .4) +
  theme_minimal()

###---------------------------------------------------------------------------###

## extract coordinates for background
# coordinates
## background coordinates
bc <- tibble::as.tibble(raster::rasterToPoints(var)[, 1:2])
bc
colnames(bc) <- c("lon", "lat")
bc

# plot
# Se for usar PCA para as variaveis, trocar o campo "fill = bio02" pelo nome do PC
ggplot() +
  geom_raster(data = raster::rasterToPoints(var[[1]]) %>% tibble::as_tibble(), 
              aes(x, y, fill = pc01)) +
  scale_fill_gradientn(colours = viridis::viridis(100)) +
  geom_point(data = bc %>% dplyr::sample_n(1000), aes(lon, lat), size = 2.5, pch = 15, alpha = .4) +
  theme_minimal()

###---------------------------------------------------------------------------###

# verify maxent
# copy maxent.jar in "C:\Users\seu_nome\Documents\R\win-library\3.5.1\dismo\java"
file.exists(paste0(system.file(package = "dismo"), "/java/maxent.jar"))

###---------------------------------------------------------------------------###

### enms ###
# diretory
setwd(choose.dir())

dir.create("03_modelos")
setwd("03_modelos")

dir.create("02_modelos_multiplos")
setwd("02_modelos_multiplos")
getwd()


# enms

for(i in 1:length(unique(occ[, 1]))){ # for to each specie
  
  # graphics
  dir.create("graphics")
  
  # variables for evaluate
  eval.Bioclim <- NULL
  eval.Gower <- NULL
  eval.GLM <- NULL
  eval.RandomForest <- NULL
  eval.Maxent <- NULL
  eval.SVM <- NULL
  eval.names <- NULL
  maxent.results <- matrix()
  
  # selecting presence and absence
  id.specie <- as.character(unique(occ[, 1]))[i]
  pr.specie <- occ[which(occ[, 1] == id.specie), 2:3]
  id.background <- sample(nrow(bc), nrow(pr.specie))
  bc.specie <- bc[id.background, ]
  
  # for
  for(r in 1:10){	# number of replicas
    
    ## preparing the models
    # train and test data	
    pr.sample.train <- sample(nrow(pr.specie), round(0.7 * nrow(pr.specie)))
    bc.sample.train <- sample(nrow(bc.specie), round(0.7 * nrow(bc.specie)))
    train <- na.omit(dismo::prepareData(x = var, p = pr.specie[pr.sample.train, ], b = bc.specie[bc.sample.train, ]))
    test <- na.omit(dismo::prepareData(x = var, p = pr.specie[-pr.sample.train, ], b = bc.specie[-bc.sample.train, ]))
    
    
    ### algorithms ###
    
    ## 1. bioclim
    print(paste(id.specie, "Bioclim", ifelse(r < 10, paste0("0", r), r)))
    Bioclim <- dismo::bioclim(train[which(train[, 1] == 1), -1])
    raster::writeRaster(dismo::predict(var, Bioclim, progress = "text"), paste0("bioclim_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff")
    eBioclim <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Bioclim)
    idBioclim <- which(eBioclim@t == as.numeric(threshold(eBioclim, "spec_sens"))) #Calcula o trashhold de maxima sensitividade e especicificidade
    eval.Bioclim.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "bioclim", eBioclim@t[idBioclim], eBioclim@auc, (eBioclim@TPR[idBioclim] + eBioclim@TNR[idBioclim] - 1))
    eval.Bioclim <- rbind(eval.Bioclim, eval.Bioclim.sp)
    
    setwd("graphics")
    tiff(paste0("bioclim_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(Bioclim); dev.off()
    tiff(paste0("bioclim_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eBioclim, "ROC"); dev.off()
    setwd("..")
    
    #Criar as pastas results e para cada cenário dentro das pastas
    
    Bioclim.cc45bi50 <- dismo::predict(Bioclim, var.cc45bi50)
    writeRaster(Bioclim.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Bioclim.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Bioclim.cc45bi50)
    
    Bioclim.cc60bi50 <- dismo::predict(Bioclim, var.cc60bi50)
    writeRaster(Bioclim.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Bioclim.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Bioclim.cc60bi50)
    
    Bioclim.cc85bi50 <- dismo::predict(Bioclim, var.cc60bi50)
    writeRaster(Bioclim.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Bioclim.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Bioclim.cc85bi50)
      
    ## 2. gower
    print(paste(id.specie, "Gower", ifelse(r < 10, paste0("0", r), r)))
    Gower <- dismo::domain(train[which(train[, 1] == 1), -1])	
    raster::writeRaster(dismo::predict(var, Gower, progress = "text"), paste0("gower_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eGower <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Gower)
    idGower <- which(eGower@t == as.numeric(threshold(eGower, "spec_sens")))
    eval.Gower.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "gower", eGower@t[idGower], eGower@auc, (eGower@TPR[idGower] + eGower@TNR[idGower] - 1))
    eval.Gower <- rbind(eval.Gower, eval.Gower.sp)
    
    setwd("graphics")
    tiff(paste0("gower_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(Gower); dev.off()
    tiff(paste0("gower_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGower, "ROC"); dev.off()
    setwd("..")
    
    Gower.cc45bi50 <- dismo::predict(Gower, var.cc45bi50)
    writeRaster(Gower.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Gower.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Gower.cc45bi50)
    
    Gower.cc60bi50 <- dismo::predict(Gower, var.cc60bi50)
    writeRaster(Gower.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Gower.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Gower.cc60bi50)
    
    Gower.cc85bi50 <- dismo::predict(Gower, var.cc60bi50)
    writeRaster(Gower.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Gower.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Gower.cc85bi50)
    
    ## 3. glm
    print(paste(id.specie, "GLM", ifelse(r < 10, paste0("0", r), r)))
    GLM <- glm(pb ~ ., data = train)	
    raster::writeRaster(dismo::predict(var, GLM, progress = "text"), paste0("glm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eGLM <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = GLM)
    idGLM <- which(eGLM@t == as.numeric(threshold(eGLM, "spec_sens")))
    eval.GLM.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "glm", eGLM@t[idGLM], eGLM@auc, (eGLM@TPR[idGLM] + eGLM@TNR[idGLM] - 1))
    eval.GLM <- rbind(eval.GLM, eval.GLM.sp)
    
    setwd("graphics")
    tiff(paste0("glm_response_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(GLM); dev.off()
    tiff(paste0("glm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eGLM, "ROC"); dev.off()
    setwd("..")
    
    GLM.cc45bi50 <- dismo::predict(GLM, var.cc45bi50)
    writeRaster(GLM.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(GLM.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(GLM.cc45bi50)
    
    GLM.cc60bi50 <- dismo::predict(GLM, var.cc60bi50)
    writeRaster(GLM.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(GLM.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(GLM.cc60bi50)
    
    GLM.cc85bi50 <- dismo::predict(GLM, var.cc60bi50)
    writeRaster(GLM.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(GLM.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(GLM.cc85bi50)
    
    ## 4. random forest
    print(paste(id.specie, "Random Forest", ifelse(r < 10, paste0("0", r), r)))
    RandomForest <- randomForest::randomForest(pb ~ ., data = train)
    writeRaster(dismo::predict(var, RandomForest, progress = "text"), paste0("randomforest_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eRandomForest <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = RandomForest)
    idRandomForest <- which(eRandomForest@t == as.numeric(threshold(eRandomForest, "spec_sens")))
    eval.RandomForest.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "randomforest", eRandomForest@t[idRandomForest], eRandomForest@auc, (eRandomForest@TPR[idRandomForest] + eRandomForest@TNR[idRandomForest] - 1))
    eval.RandomForest <- rbind(eval.RandomForest, eval.RandomForest.sp)
    
    setwd("graphics")
    tiff(paste0("randomforest_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eRandomForest, "ROC"); dev.off()
    setwd("..")
    
    RandomForest.cc45bi50 <- dismo::predict(RandomForest, var.cc45bi50)
    writeRaster(RandomForest.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(RandomForest.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(GLM.cc45bi50)
    
    RandomForest.cc60bi50 <- dismo::predict(GLM, var.cc60bi50)
    writeRaster(RandomForest.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(RandomForest.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(RandomForest.cc60bi50)
    
    RandomForest.cc85bi50 <- dismo::predict(RandomForest, var.cc60bi50)
    writeRaster(RandomForest.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(RandomForest.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(RandomForest.cc85bi50)
    
    ## 5. maxent	
    print(paste(id.specie, "Maxent", ifelse(r < 10, paste0("0", r), r)))
    Maxent <- dismo::maxent(train[, -1], train[, 1])	
    raster::writeRaster(dismo::predict(var, Maxent, progress = "text"), paste0("maxent_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eMaxent <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = Maxent)
    idMaxent <- which(eMaxent@t == as.numeric(threshold(eMaxent, "spec_sens")))
    eval.Maxent.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "maxent", eMaxent@t[idMaxent], eMaxent@auc, (eMaxent@TPR[idMaxent] + eMaxent@TNR[idMaxent] - 1))
    eval.Maxent <- rbind(eval.Maxent, eval.Maxent.sp)
    
    setwd("graphics")
    tiff(paste0("maxent_response_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); dismo::response(Maxent); dev.off()
    tiff(paste0("maxent_contribution_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(Maxent); dev.off()
    tiff(paste0("maxent_auc_",  id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eMaxent, "ROC"); dev.off()
    maxent.results <- tibble::as.tibble(data.frame(maxent.results, as.matrix(Maxent@results)))
    setwd("..")
    
    Maxent.cc45bi50 <- dismo::predict(Maxent, var.cc45bi50)
    writeRaster(Maxent.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Maxent.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Maxent.cc45bi50)
    
    Maxent.cc60bi50 <- dismo::predict(Maxent, var.cc60bi50)
    writeRaster(Maxent.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Maxent.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Maxent.cc60bi50)
    
    Maxent.cc85bi50 <- dismo::predict(Maxent, var.cc60bi50)
    writeRaster(Maxent.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(Maxent.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(Maxent.cc85bi50)
    
    ## 6. svm	
    print(paste(id.specie, "SVM", ifelse(r < 10, paste0("0", r), r)))
    SVM <- kernlab::ksvm(pb ~ ., data = train)
    raster::writeRaster(dismo::predict(var, SVM, progress = "text"), paste0("svm_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif"), options = c("COMPRESS=DEFLATE"), format = "GTiff") 
    eSVM <- dismo::evaluate(p = test[test[, 1] == 1, -1], a = test[test[, 1] == 0, -1], model = SVM)
    idSVM <- which(eSVM@t == as.numeric(threshold(eSVM, "spec_sens")))
    eval.SVM.sp <- c(id.specie, ifelse(r < 10, paste0("0", r), r), "svm", eSVM@t[idSVM], eSVM@auc, (eSVM@TPR[idSVM] + eSVM@TNR[idSVM] - 1))
    eval.SVM <- rbind(eval.SVM, eval.SVM.sp)
    
    setwd("graphics")
    tiff(paste0("svm_auc_", id.specie, ifelse(r < 10, paste0("0", r), r), ".tif")); plot(eSVM, "ROC"); dev.off()
    setwd("..")
    
    SVM.cc45bi50 <- dismo::predict(SVM, var.cc45bi50)
    writeRaster(SVM.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(SVM.cc45bi50,filename=paste(getwd(),"/Results/cc45bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(SVM.cc45bi50)
    
    SVM.cc60bi50 <- dismo::predict(SVM, var.cc60bi50)
    writeRaster(SVM.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(SVM.cc60bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(SVM.cc60bi50)
    
    SVM.cc85bi50 <- dismo::predict(SVM, var.cc60bi50)
    writeRaster(SVM.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".grd", sep=""), overwrite=T)
    writeRaster(SVM.cc85bi50,filename=paste(getwd(),"/Results/cc60bi50/",splist[i], ".tif", sep=""), overwrite=T)
    rm(SVM.cc85bi50)
    
    eval.names <- c(eval.names, paste0(id.specie, ifelse(r < 10, paste0("0", r), r)))	
    
  } # ends for "r"
  
  # maxent results
  setwd("graphics")
  na <- attributes(Maxent@results)[[2]][[1]]
  maxent.results <- tibble::as.tibble(data.frame(na, maxent.results[, -1]))
  colnames(maxent.results) <- c("names", paste0("rep", 1:r))
  readr::write_csv(maxent.results, paste0("_maxent_results", id.specie, ".csv"))
  setwd("..")
  
  # evaluations
  dimnames(eval.Bioclim) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  dimnames(eval.Gower) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))  
  dimnames(eval.GLM) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))  
  dimnames(eval.RandomForest) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  dimnames(eval.Maxent) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  dimnames(eval.SVM) <- list(eval.names, c("species", "replica", "algorithm", "thrs", "AUC", "TSS"))
  
  write.csv(eval.Bioclim, paste0("zEval_", "bioclim_", id.specie, ".csv"))
  write.csv(eval.Gower, paste0("zEval_", "gower_", id.specie, ".csv"))
  write.csv(eval.GLM, paste0("zEval_", "glm_", id.specie, ".csv"))
  write.csv(eval.RandomForest, paste0("zEval_", "randomforest_", id.specie, ".csv"))
  write.csv(eval.Maxent, paste0("zEval_", "maxent_", id.specie, ".csv"))
  write.csv(eval.SVM, paste0("zEval_", "svm_", id.specie, ".csv"))
  
} # ends for"i"

###----------------------------------------------------------------------------###

