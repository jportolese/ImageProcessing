library(rgdal)
library(raster)
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

ls.B5hist <- hist(ls.B5@data$band1,
                  breaks =10,
                  main = "Frequecy Distribution of pixels in\n Near-Infrared Band (NIR)",
                  col = "wheat3",
                  xlab = "Reflectance in NIR (DN values)")

# clip each spatial grid dataframe using a shapefile

# load shapefile
StudyArea <- readOGR("C:/JP/DataScienceClasses/Capstone", "StudyArea1")
StudyArea
ls.B1@proj4string

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

plotRGB( SubsetImg * (SubsetImg >= 0), r = 5, g = 3, b = 2, scale = 12000
         , stretch ='lin')

plot(ls.B1, breaks = seq(0, 14000, 2000), col= 3,
           main="Blue Band of the Landsat Image")
plot(StudyArea)
hist(B1_Subset)
nrow(B1_Subset)
dim(ls.B1)
