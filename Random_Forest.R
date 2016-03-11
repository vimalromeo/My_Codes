library(randomForest)
library(caTools)
stevens <- read.csv("stevens.csv")
str(stevens)
#splitting the data to train and test
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
Train <- subset(stevens, spl == TRUE)
Test <- subset(stevens, spl == FALSE)
#Creating the Forest
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent
                              + LowerCourt + Unconst, data = Train,
                              nodesize = 25, ntree = 200)
#random forest does not have the argument to specify regression or classification
#We need to have the response variable as factor for doing a classification
Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent
                              + LowerCourt + Unconst, data = Train,
                              nodesize = 25, ntree = 200)
#Letz make predictions
PredictForest <- predict(StevensForest, newdata = Test)
#Confusion matrix for accuracy
table(Test$Reverse, PredictForest)
(41+74)/(41+36+19+74)
#Got a slightly different value than online course because of the random component
