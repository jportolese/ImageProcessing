#  This section of code will use R to import training areas (from a feature class) 
# and use the pixels in these training areas to train the random forest 
# algorithm to identify land cover.


# Load Training Pixels - fgdb is the connection to the ESRI file Geodatabase set up earlier
TrainingPixels <- readOGR(dsn=fgdb, "MVTrainingAreas")
TrainingPixels


# extract 7 bands of info from the training pixels and put them in dfTraining spatial data.frame

# field in the feature class that identifies landcover type
responseCol <- "CovCode"

# load the training pixels into dfTraining.

dfTraining <- data.frame(matrix(vector(), nrow = 0, ncol = length(names(MVSubsetImg)) + 1))   
for (i in 1:length(unique(TrainingPixels[[responseCol]]))){
  category <- unique(TrainingPixels[[responseCol]])[i]
  categorymap <- TrainingPixels[TrainingPixels[[responseCol]] == category,]
  dataSet <- extract(MVSubsetImg, categorymap)
  dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
  dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
  df <- do.call("rbind", dataSet)
  dfTraining <- rbind(dfTraining, df)
  }



# use Caret package to apply randomforest to training set.  Using all 7 bands 

library(caret)
rfmod1_7 <- train(as.factor(class)~ B1 + B2 + B3 + B4 + B5 + B6 + B7, method = "rf", data = dfTraining)


# use model to predict the test set.
LC_Preds_MV <- predict(MVSubsetImg, model = rfmod1_7)
