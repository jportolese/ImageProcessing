
library(caret) #required for confusionMatrix()

# import the ground truth image for actual landcover in the test area
GroundTruth <- readGDAL("C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\GroundTruth1.img")
GroundTruth <- as.vector(GroundTruth$band1)

#Convert classified image to a vector for comparison
predictions <- as.integer(as.vector(LC_Preds_test$layer)) #values from classification
#GroundTruth <- GroundTruth #reference values (observed/checked) for validation 


table(GroundTruth,predictions) #shows confusion matrix
dimnames(tab)[[1]] = c("Impervious","Developed Open Space", "Cultivated Land", "Pasture Hay", "Grassland", "Deciduous Forest",
                       "Evergreen Forest","Scrub/Shrub", "Pal For Wetland", "Pal Scrub Wetland", "Pal Emerge Wetland", 
                       "Est Emerge Wetland", "Unconsolidated Shore", "Bare Ground", "Water")


conmat <- confusionMatrix(table(GroundTruth,predictions))
write.csv(table(GroundTruth,predictions), "C:\\JP\\DataScienceClasses\\Capstone\\conmat.csv")

table(GroundTruth)