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


# Create a raster stack image of all 7 bands of the subsetted image
FullImage <- stack(raster(new_B1), raster(new_B2), raster(new_B3)
                   , raster(new_B4), raster(new_B5), raster(new_B6), 
                   raster(new_B7))
# Rename the bands to B1 - B7
names(FullImage) <- paste0("B", c(1:7))


# plots a single band 5 image panchromatic of the entire original image
plot(new_B5,col = gray(0:100 / 100))


# load file geodatabase feature class boundary for Martha's Vineyard
fgdb <- "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\lclu_gdb\\MA_LCLU2016.gdb"


# Load Area Boundary from file geodatabase
MVSubsetBND <- readOGR(dsn=fgdb, layer="MV_Subset")


# Create the Study area subset image that will be used in the project
MVSubsetImg <- crop(FullImage, MVSubsetBND)


# True color plot of Martha's Vineyard Image
plotRGB(MVSubsetImg * (MVSubsetImg >= 0)
        , scale = 12000
        , r = 4, g = 3, b = 2
        , stretch ='lin')

# False color Color Infrared plot of Martha's Vineyard Image
plotRGB(MVSubsetImg * (MVSubsetImg >= 0)
        , r = 5, g = 4, b = 3
        , scale = 12000
        , stretch ='lin')
