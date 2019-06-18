
library(caret) #required for confusionMatrix()

# import the ground truth image for actual landcover in the test area
GroundTruthImg <- readGDAL("C:\\JP\\DataScienceClasses\\Capstone\\FinalGroundTruth.img")
GroundTruth <- as.vector(GroundTruthImg$band1)


# Export the raster layers of ground truth and final model classified image.
writeRaster(LC_Preds_MV, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\ClassifiedMV.img", overwrite = TRUE)
writeRaster(GroundTruthImg, "C:\\JP\\DataScienceClasses\\Capstone\\LandCover\\GroundTruth.img")

# create a confusion between the groundtruth and predicted vectors
confusionMatrix(table(GroundTruth,predictions))

# write the results to a csv for reformatting in excel.
write.csv(confusionMatrix(table(GroundTruth,predictions)), "C:\\JP\\DataScienceClasses\\Capstone\\ConfusionMatrixfinal.csv")
