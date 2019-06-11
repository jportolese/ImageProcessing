library(ggplot2)
library(dplyr)
library(magrittr)

Impervious <- dfTraining %>% filter(class == 2)
boxplot(Impervious, names = c("B1", "B2", "B3", "B4", "B5", "B6", "B7"))