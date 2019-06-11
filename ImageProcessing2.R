library(rgdal)
library(raster)
library(caret)
path <- "C:/JP/USGS/LC08_07132016"
setwd(path)

# Import each band into a separate spatial grid dataframe

new_B1 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B1.tif")
new_B2 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B2.tif")
new_B3 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B3.tif")
new_B4 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B4.tif")
new_B5 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B5.tif")
new_B6 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B6.tif")
new_B7 <- readGDAL("LC08_L1TP_012031_20160713_20180130_01_T1_B7.tif")


# clip each spatial grid dataframe using a shapefile


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



# Create a raster stack image of all 7 bands of the subsetted image
NewSubsetImg <- stack(B1_SubNew, B2_SubNew, B3_SubNew, B4_SubNew, B5_SubNew
                   , B6_SubNew, B7_SubNew)
names(NewSubsetImg) <- paste0("B", c(1:7))

# Create same as above but with only MV area
MVSubsetImg <- stack(B1_SubMV, B2_SubMV, B3_SubMV, B4_SubMV, B5_SubMV
                      , B6_SubMV, B7_SubMV)
names(MVSubsetImg) <- paste0("B", c(1:7))


# this code plots a true color image or the final study area
plotRGB(NewSubsetImg * (NewSubsetImg >= 0), r = 4, g = 3, b = 2, scale = 12000
        , stretch ='lin')

# this code plots a true color image or the final study area
plotRGB(MVSubsetImg * (MVSubsetImg >= 0), r = 4, g = 3, b = 2, scale = 12000
        , stretch ='lin')

writeRaster(NewSubsetImg, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\raw0713.img")

names(NewSubsetImg) <- paste0("B", c(1:7)) 