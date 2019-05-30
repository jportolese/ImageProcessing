library(rgdal)
library(raster)
library(caret)
path <- "C:/JP/USGS/"
setwd(path)

# Import each band into a separate spatial grid dataframe

ls.B1 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band1.tif")
ls.B2 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band2.tif")
ls.B3 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band3.tif")
ls.B4 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band4.tif")
ls.B5 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band5.tif")
ls.B6 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band6.tif")
ls.B7 <- readGDAL("LC08_L1TP_012031_20160424_20170223_01_T1_sr_band7.tif")

slotNames(ls.B1)
summary(ls.B5@data)
ls.B5@proj4string
ls.B5@bbox
ls.B5@grid
ls.B5@data

# plot band 5
ls.B5.1 <- raster(ls.B5)
plot(ls.B5.1,
     main="Near Infrared Band of the Landsat Image")

plot(B2_Subset, 
     main= "Band 2 of Subsetted image")


ls.B5hist <- hist(ls.B5@data$band1,
                  breaks =10,
                  main = "Frequecy Distribution of pixels in\n Near-Infrared Band (NIR)",
                  col = "wheat3",
                  xlab = "Reflectance in NIR (DN values)")

# clip each spatial grid dataframe using a shapefile

# load shapefiles
StudyArea <- readOGR("C:\\JP\\DataScienceClasses\\Capstone", "StudyArea1")
StudyArea


# load the training area shapefile

TrainSets <- readOGR("C:\\JP\\DataScienceClasses\\Capstone\\LandCover", "TrainAreas")
TrainSets


# Subset band1 using the StudyArea layer
library(raster)
rgrid <- raster(ls.B1)
B1_Subset <- crop(rgrid, StudyArea)
summary(B1_Subset)


rgrid <- raster(ls.B2)
B2_Subset <- crop(rgrid, StudyArea)
summary(B2_Subset)

rgrid <- raster(ls.B3)
B3_Subset <- crop(rgrid, StudyArea)
summary(B3_Subset)

rgrid <- raster(ls.B4)
B4_Subset <- crop(rgrid, StudyArea)
summary(B4_Subset)

rgrid <- raster(ls.B5)
B5_Subset <- crop(rgrid, StudyArea)
summary(B5_Subset)

rgrid <- raster(ls.B6)
B6_Subset <- crop(rgrid, StudyArea)
summary(B6_Subset)

rgrid <- raster(ls.B7)
B7_Subset <- crop(rgrid, StudyArea)
summary(B7_Subset)

SubsetImg <- stack(B1_Subset, B2_Subset, B3_Subset, B4_Subset, B5_Subset
              , B6_Subset, B7_Subset)

names(SubsetImg) <- paste0("B", c(1:7)) 
class(SubsetImg)

SubsetImg[45, 1]
summary(SubsetImg$B4)

plotRGB( SubsetImg * (SubsetImg >= 0), r = 4, g = 3, b = 2, scale = 12000
         , stretch ='lin')


responseCol <- "covcode"

dfAll <- data.frame(matrix(vector(), nrow = 0, ncol=length(names(SubsetImg))+ 1))
for (i in 1:length(unique(TrainSets[[responseCol]]))){
  category <- unique(TrainSets[[responseCol]])[i]
  categorymap <- TrainSets[TrainSets[[responseCol]] == category,]
  dataset <- extract(SubsetImg, categorymap)
  dataset <- sapply(dataset, function(x){cbind(x, class = rep(category, nrow(x)))})
  df <- do.call("rbind", dataset)
  dfAll <- rbind(dfAll, df)
}

dfAll

library(dplyr)
class6 <- filter(dfAll, class == 6)
class21 <- filter(dfAll, class == 21)


# run some r to get the standard deviation of each band by landcover class to identify training pixels that have wide variations

Summarytable <- table(dfAll$class)
Summarytable

B1 <- class6$B1
B2 <- class6$B2
B3 <- class6$B3
B4 <- class6$B4
B5 <- class6$B5
B6 <- class6$B6
B7 <- class6$B7

plot(B1, B2)
plot(B2, B3)
plot(B2, B5)
plot(B4, B5, main = "Band4 vs Band 5 Bare Ground"
     , xlab = "Band 4", ylab = "Band 5",
     pch = 15, frame = FALSE)
abline(lm(B5~B4, data = class2), col= "blue")
plot(B2, B6, main = "Band2 vs Band 6"
     , xlab = "Band 2", ylab = "Band 6",
     pch = 15, frame = FALSE)
abline(lm(B2~B6, data = class6), col= "blue")

# apply randomforest to training set 
library(caret)
mod <- train(as.factor(class)~ B1 + B2 + B3 + B4 + B5 + B6 + B7, method = "rf", data = dfAll)
mod

beginCluster()
preds_rf <- clusterR(SubsetImg, raster::predict, args = list(model = mod))
endCluster()

preds_rf
plot(preds_rf)
names(preds_rf)

freq(preds_rf)

#Accuracy Assessment

# load Land Cover feature class from file geodatabase

fgdb <- "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\lclu_gdb\\MA_LCLU2016.gdb"
LC_fc <- readOGR(dsn=fgdb,layer="LULC_Test_Fix1")
TestArea <- readOGR(dsn=fgdb, layer = "TestAreaBnd")
names(LC_fc)
crs(LC_fc)
extent(LC_fc)

# create a test subset image of the final predictions
# for comparison to actual Landcover data

preds_rf_test <- crop(preds_rf, TestArea)
preds_rf_test

# got the row and column count from prediction image use it
# to create an empty raster to rasterize the Land Cover 
# Check image LC_fc

LC_GroundTruth <- raster(nrow = 731, ncol = 1497
                         , resolution =30, crs = 
                           "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs
                         +ellps=WGS84 +towgs84=0,0,0"
                         , xmn = 292828.3
                         , ymn = 4674141
                         , xmx = 337726.8 
                         , ymx = 4696055)

# Run this using parallel processing for speed.
beginCluster()
GroundTruthImage <- rasterize(LC_fc, LC_GroundTruth, "COVERCODE")
endCluster()

writeRaster(GroundTruthImage, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\LC_GroundTruth.img")
writeRaster(preds_rf_test,"C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\classifiedImage.img" )

ncol(GroundTruthImage)
plot(GroundTruthImage)

freq(preds_rf_test)
freq(GroundTruthImage)
