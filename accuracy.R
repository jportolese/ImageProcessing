
library(caret) #required for confusionMatrix()

# import the ground truth image for actual landcover in the test area
GroundTruthImg <- readGDAL("C:\\JP\\DataScienceClasses\\Capstone\\Rec_GrndTruth.img")
GroundTruth <- as.vector(GroundTruthImg$band1)

aggregate(GroundTruthImg$band1)


writeRaster(LC_Preds_MV, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\ClassifiedMV.img")
table(GroundTruth)

#Convert classified image to a vector for comparison
predictions <- as.integer(as.vector(LC_Preds_MV$layer)) #values from classification
#GroundTruth <- GroundTruth #reference values (observed/checked) for validation 

length(predictions)
table(predictions)
table(GroundTruth)


GroundTruth[is.na(GroundTruth[])] <- 21 
length(GroundTruth)

table(GroundTruth,predictions) #shows confusion matrix
dimnames(tab)[[1]] = c("Impervious","Developed Open Space", "Cultivated Land", "Pasture Hay", "Grassland", "Deciduous Forest",
                       "Evergreen Forest","Scrub/Shrub", "Pal For Wetland", "Pal Scrub Wetland", "Pal Emerge Wetland", 
                       "Est Emerge Wetland", "Unconsolidated Shore", "Bare Ground", "Water")


conmatTest <- confusionMatrix(table(GroundTruth,predictions))
write.csv(table(GroundTruth,predictions), "C:\\JP\\DataScienceClasses\\Capstone\\ConfusionMatrix.csv")

table(GroundTruth)