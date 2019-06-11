#  This section of code will use R to import training areas (from shapefiles) 
# and use the pixels in these training areas to train the random forest 
# algorithm to identify what land cover exists at these locations.

# The landcover types I'm trying to identify are the following:

# Impervious Surface (concrete, rooftops, etc.)
# Grassland / Pasture / open Space
# Cultivated Land
# Deciduous Forest
# Evergreen Forest
# Scrub/Shrub
# Palustrine Wetland
# Estuarine Wetland
# Bare Land (no vegetaion, sand exposed rock etc.)
# Water

# 


# load feature classes from file geodatabase
# load file geodatabase feature class boundary for Martha's Vineyard
fgdb <- "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\lclu_gdb\\MA_LCLU2016.gdb"



# Load Training Pixels 
# load the training area shapefile


TrainingPixels <- readOGR(dsn=fgdb, "MVTrainingAreas")
TrainingPixels

table(TrainingPixels$CovCode)


# extract 7 bands of info from the training pixels and put them in dfTraining
# new code

responseCol <- "CovCode"

dfTraining = data.frame(matrix(vector(), nrow = 0, ncol = length(names(MVSubsetImg)) + 1))   
for (i in 1:length(unique(TrainingPixels[[responseCol]]))){
  category <- unique(TrainingPixels[[responseCol]])[i]
  categorymap <- TrainingPixels[TrainingPixels[[responseCol]] == category,]
  dataSet <- extract(MVSubsetImg, categorymap)
  if(is(TrainingPixels, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfTraining <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(TrainingPixels, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfTraining <- rbind(dfAll, df)
  }
}



table(dfTraining)
class(dfTraining)
pf <- subset(dfTraining, class == 13)
write.csv(EE, "C:\\temp\\EE.csv")
hist(pf$band1.5)



library(dplyr)
class6 <- filter(dfTraining, class == 6)
class18 <- filter(dfTraining, class == 18)


# run some r to get the standard deviation of each band by landcover class to identify training pixels that have wide variations

Summarytable <- table(dfTraining$class)
Summarytable

B1 <- class18$B1
B2 <- class18$B2
B3 <- class18$B3
B4 <- class18$B4
B5 <- class18$B5
B6 <- class18$B6
B7 <- class18$B7

hist(B7)

plot(B1, B2)
plot(B2, B3)
plot(B2, B5)
plot(B4, B5, main = "Band4 vs Band 5 Bare Ground"
     , xlab = "Band 4", ylab = "Band 5",
     pch = 15, frame = TRUE,
     abline(lm(B5~B4, data = class18), col= "blue"))
plot(B2, B6, main = "Band2 vs Band 6"
     , xlab = "Band 2", ylab = "Band 6",
     pch = 15, frame = FALSE,
     abline(lm(B2~B6, data = class18), col= "blue"))


#Accuracy Assessment

names(dfTraining)
write.csv(dfTraining, "C:\\JP\\DataScienceClasses\\Capstone\\trainingpixels.csv")
names(dfTraining) <- paste0("B", c(1:7))
names(dfTraining) <- paste0("B", c(1:7))


# apply randomforest to training set.  Using all 7 bands  
library(caret)
rfmod1_7 <- train(as.factor(class)~ B1 + B2 + B3 + B4 + B5 + B6 + B7, method = "rf", data = dfTraining)
rfmod1_7





# use rfmod1_7 to classify the entire image
#beginCluster()
#LC_Preds <- clusterR(NewSubsetImg, raster::predict, args = list(model = rfmod1_7))
#endCluster()

# use rfmod1_7 to classify the MV image
beginCluster()
LC_Preds_MV <- clusterR(MVSubsetImg, raster::predict, args = list(model = rfmod1_7))
endCluster()

nrow(LC_Preds_MV)
ncol(LC_Preds_MV)


plot(LC_Preds)

freq(LC_Preds)

writeRaster(LC_Preds_MV, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\classifiedMVImg.img", overwrite=TRUE)

writeRaster(LC_Preds, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\classified.img", overwrite=TRUE)


plot(LC_Preds543)