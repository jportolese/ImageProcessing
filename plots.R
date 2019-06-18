library(ggplot2)
library(dplyr)

# add an ID to each pixel in the training 
# dataframe for plotting

# add an ID to dfTraining for plotting
ID = seq(1:nrow(dfTraining))
dfTraining <- cbind(dfTraining, ID)

# load simple lookup table created in excel for the landcover classes and merge
#df training to show the class names in the final plot
lookup <- read.csv(file="C:\\JP\\DataScienceClasses\\Capstone\\MyProject\\images\\lookup.csv", header=TRUE)
TrainingSignatures <- merge(dfTraining, lookup, by.x = "class", by.y="LCID")

# load tidyverse to use the gather command
library(tidyr)

test %>% 
  gather(col, val, -c(classname, ID, class)) %>%  # from `tidyr`
  ggplot(aes(col, val, color = ID, group = ID)) + 
  geom_line() +
  facet_wrap(~classname)



