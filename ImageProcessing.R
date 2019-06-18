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


# plot band 5
ls.B5.1 <- raster(ls.B5)
plot(ls.B5.1,
     main="Near Infrared Band of the Landsat Image")



ls.B5hist <- hist(ls.B5@data$band1,
                  breaks =10,
                  main = "Frequency Distribution of pixels in\n Near-Infrared Band (NIR)",
                  col = "wheat3",
                  xlab = "Reflectance in NIR (DN values)")

# clip each spatial grid dataframe using a shapefile



# load file geodatabase feature class boundary for Martha's Vineyard
fgdb <- "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\lclu_gdb\\MA_LCLU2016.gdb"

# Load landcover ground truth data
LC_fc <- readOGR(dsn=fgdb,layer="LULC_Test_Fix1")
names(LC_fc)

# Load Area Boundary
# load shapefiles for the cropped study area
StudyArea <- readOGR(dsn=fgdb, layer="StudyArea1")
TestArea <- readOGR(dsn=fgdb, layer = "TestAreaBnd")
MVSubsetBND <- readOGR(dsn=fgdb, layer="MV_Subset")

# Subset each band using the StudyArea layer 
library(raster)
rgrid <- raster(ls.B1)
B1_Subset <- crop(rgrid, StudyArea)
#summary(B1_Subset)


rgrid <- raster(ls.B2)
B2_Subset <- crop(rgrid, StudyArea)
#summary(B2_Subset)

rgrid <- raster(ls.B3)
B3_Subset <- crop(rgrid, StudyArea)
#summary(B3_Subset)

rgrid <- raster(ls.B4)
B4_Subset <- crop(rgrid, StudyArea)
#summary(B4_Subset)

rgrid <- raster(ls.B5)
B5_Subset <- crop(rgrid, StudyArea)
#summary(B5_Subset)

rgrid <- raster(ls.B6)
B6_Subset <- crop(rgrid, StudyArea)
#summary(B6_Subset)

rgrid <- raster(ls.B7)
B7_Subset <- crop(rgrid, StudyArea)
#summary(B7_Subset)


# Create a raster stack image of all 7 bands of the subsetted image
SubsetImg <- stack(B1_Subset, B2_Subset, B3_Subset, B4_Subset, B5_Subset
              , B6_Subset, B7_Subset)

#subset the Test image and MV image
MVSubset <- crop(SubsetImg, MVSubsetBND)
plot(MVSubset)
writeRaster(MVSubset, "C:\\JP\\DataScienceClasses\\Capstone\\RawImagery\\MVRaw.img")

plotRGB( MVSubset * (MVSubset >= 0), r = 4, g = 3, b = 2, scale = 12000
         , stretch ='lin')

# change the names of the individual bands to B1:7
names(SubsetImg) <- paste0("B", c(1:7)) 



# this code plots a true color image or the final study area
plotRGB( SubsetImg * (SubsetImg >= 0), r = 4, g = 3, b = 2, scale = 12000
         , stretch ='lin')





# Subset each band using the StudyArea layer 
library(raster)
rgrid <- raster(new_B1)
B1_SubNew <- crop(rgrid, StudyArea)
#summary(B1_Subset)


rgrid <- raster(new_B2)
B2_SubNew <- crop(rgrid, StudyArea)
#summary(B2_Subset)

rgrid <- raster(new_B3)
B3_SubNew <- crop(rgrid, StudyArea)
#summary(B3_Subset)

rgrid <- raster(new_B4)
B4_SubNew <- crop(rgrid, StudyArea)
#summary(B4_Subset)

rgrid <- raster(new_B5)
B5_SubNew <- crop(rgrid, StudyArea)
#summary(B5_Subset)

rgrid <- raster(new_B6)
B6_SubNew <- crop(rgrid, StudyArea)
#summary(B6_Subset)

rgrid <- raster(new_B7)
B7_SubNew <- crop(rgrid, StudyArea)
#summary(B7_Subset)


# MV image for classification

# Subset each band using the MV (test) study area layer 
library(raster)
rgrid <- raster(new_B1)
B1_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B1_Subset)


rgrid <- raster(new_B2)
B2_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B2_Subset)

rgrid <- raster(new_B3)
B3_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B3_Subset)

rgrid <- raster(new_B4)
B4_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B4_Subset)

rgrid <- raster(new_B5)
B5_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B5_Subset)

rgrid <- raster(new_B6)
B6_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B6_Subset)

rgrid <- raster(new_B7)
B7_SubMV <- crop(rgrid, MVSubsetBND)
#summary(B7_Subset)












LC_Preds
plot(LC_Preds)
names(LC_Preds)

freq(LC_Preds)


# create a test subset image of the final predictions
# for comparison to actual Landcover data

LC_Preds_test <- crop(LC_Preds, TestArea)
LC_Preds_test
LC_Preds_MV <- crop(LC_Preds, MVSubsetBND)
class(LC_Preds_test)
plot(LC_Preds_test)
names(LC_Preds_test)
nrow(LC_Preds_test)
LC_Preds_test2 <- LC_Preds_test[-731,]

# got the row and column count from prediction image use it
# to an empty raster to rasterize the Land Cover 
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
writeRaster(LC_Preds_test,"C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\classifiedImage.img", overwrite = TRUE )
writeRaster(SubsetImg,"C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\raw.img", overwrite = TRUE )
writeRaster(LC_Preds_MV,"C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\classifiedImage_MV.img", overwrite = TRUE )
writeRaster(SubsetImg,"C:\\JP\\DataScienceClasses\\Capstone\\SubsetIMGRaw.grd")



ncol(GroundTruthImage)
plot(GroundTruthImage)

freq(LC_Preds_test)
plot(LC_Preds_test)



GroundTruth <- readGDAL("C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\GroundTruth1.img")
class(GroundTruth)
GroundTruth <- as.raster(GroundTruth)
# Accuracy Assessment










library(dismo)
set.seed(99)
j <- kfold(dfTraining, k = 5, by=dfTraining$class)
table(j)

library(rpart)
x <- list()
for (k in 1:5) {
  train <- dfTraining[j!= k, ]
  test <- dfTraining[j == k, ]
  cart <- rpart(as.factor(class)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$class, as.integer(pclass))
}

y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- dfAll$classnames
#rownames(conmat) <-dfAll$classnames
conmat
write.csv(conmat, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\conmat.csv") 

Summarytable <- table(dfTraining$class)
Summarytable

# Accuracy Assessment

library(caret) #required for confusionMatrix()

#example values
predictions <- #values from classification
Truth <-  #reference values (observed/checked) for validation 
  
table(a,b) #shows confusion matrix
confusionMatrix(table(a,b)) #confusion matrix with Accuracy, kappa ....
