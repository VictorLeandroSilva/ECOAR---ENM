## Cortar a modelagem 

setwd(choose.dir())
getwd()
dir()

library(raster)

# check loaded packeges
search()

ti <- dir(patt = ".tif$")
plot(ti)

#importante shape
setwd(choose.dir())
dir()
alt <- shapefile("./Biomas_brasileiros")
plot(alt)
MataAltantica <- alt[alt$CD_LEGEN1 == "MATA ATLÂNTICA",]
# mo : modelo depois do essemble 

corte <- crop(mo,MataAltantica)
