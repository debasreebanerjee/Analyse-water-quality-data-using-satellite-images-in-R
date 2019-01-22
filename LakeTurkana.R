install.packages("raster")
library(raster)
install.packages("rgdal")
library(rgdal)
require(raster)
install.packages("sp")
library(sp)
library(RStoolbox)

#######Reading the images of two portion of the lake from metadata

meta_20151230_1 <- readMeta("/Users/debasreebanerjee/Desktop/LC081690572015123001T1-SC20170613105829/LC08_L1TP_169057_20151230_20170331_01_T1_MTL.txt")
summary(meta_20151230_1)

meta_20151230_2 <- readMeta("/Users/debasreebanerjee/Desktop/LC081690582015123001T1-SC20170613111254/LC08_L1TP_169058_20151230_20170331_01_T1_MTL.txt")
summary(meta_20151230_2)

######Stacking the metadata

LC08_20151230_1 <- stackMeta(meta_20151230_1)
LC08_20151230_2 <- stackMeta(meta_20151230_2)


######Haze Correction 

haze <- estimateHaze(LC08_20151230_1, darkProp = 0.01, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn", "B5_dn", "B6_dn"))


LC08_20151230_1 <- radCor(LC08_20151230_1, metaData = meta_20151230_1, hazeValues = haze, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn", "B5_dn", "B6_dn"), method = "sdos")

haze <- estimateHaze(LC08_20151230_2, darkProp = 0.01, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn", "B5_dn", "B6_dn"))

LC08_20151230_2 <- radCor(LC08_20151230_2, metaData = meta_20151230_2, hazeValues = haze, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn", "B5_dn", "B6_dn"), method = "sdos")

#########Mosaic two parts of the lake 

LC08_20151230 <- mosaic(LC08_20151230_1, LC08_20151230_2, fun=mean)

LC08_20151230

library(RStoolbox)
ggRGB(LC08_20151230, r = 4, g = 3, b = 2, stretch = "lin")

######Cloud Mask
cmsk = cloudMask(LC08_20151230, threshold = 0.8, blue = "layer.2", tir = "layer.9")
plot(cmsk)
LC08_20151230_new <- mask(LC08_20151230, cmsk$CMASK, maskvalue = NA, inverse = TRUE)

#####MNDWI Calculation 

LC08_20151230_mndwi <- (LC08_20151230_new$layer.3-LC08_20151230_new$layer.6)/(LC08_20151230_new$layer.3+LC08_20151230_new$layer.6)
plot(LC08_20151230_mndwi)


#####Removing negative values of MNDWI

LC08_20151230_mndwi[LC08_20151230_mndwi<0] <- NA
plot(LC08_20151230_mndwi)


######Chlorophyll map 

Chlorophyll = exp((-1.45)*(log(LC08_20151230_new$layer.2/LC08_20151230_new$layer.5))+4.93)
plot(Chlorophyll5)

#####Mask with MNDWI

Chlorophyllmasked <- mask(Chlorophyll,LC08_20151230_mndwi)
plot(Chlorophyllmasked)
cellStats(Chlorophyllmasked, mean)

######Reading Kenya lakes Shapefile 

install.packages("rgdal")
library(rgdal)

LakeTurkana <- readOGR("/Users/debasreebanerjee/Desktop/Dissertation/KEN_Lakes/KEN_Lakes.shp")
plot(LakeTurkana)

#####Subset Shapefile with lake Turkana

LakeTurkana_only <- subset(LakeTurkana, LakeTurkana$LAKE == 42462)
plot(LakeTurkana_only)


#####Reprojection of Raster layer

Chlorophyllmasked.new <- projectRaster(Chlorophyllmasked, crs = "+proj=longlat +ellps=WGS84 +no_defs")


#####Cropping Raster layer with lake Turkana shapefile

Chlorophyll_map <- crop(Chlorophyllmasked.new, LakeTurkana_only)
plot(Chlorophyll_map)

#####Mean Surface Reflectance
cellStats(Chlorophyll_map, mean)
summary(Chlorophyll_map_m)

######Producing Chlorophyll map in ggplot

install.packages("rasterVis")
library(rasterVis)
library(ggplot2)

gplot(Chlorophyll_map_m) + geom_tile(aes(fill=value)) + scale_fill_gradient(low = 'blue', high = 'red', na.value='transparent',name="Chl-a (ug/l)")+coord_equal()+theme_bw()

install.packages("maps")
library(maps)
map.scale(36, 2.5, relwidth = 0.15, metric = TRUE, ratio = TRUE)
install.packages("SDMTools")
library(SDMTools)
Scalebar(x=35.75, y=2.25, distance=20, unit = "km", scale = 1, t.cex =10)

