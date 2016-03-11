library(caret)
library(e1071)
library(rpart)
stevens <- read.csv("stevens.csv")
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
Train <- subset(stevens, spl == TRUE)
Test <- subset(stevens, spl == FALSE)
Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)
#Creating cross validation
#Determining the no of folds needed. We can do this using the trainControl function
#cv for cross validation and number = 10 for 10 folds
numFolds <- trainControl(method = "cv", number = 10)
#Then we need to pick the possible values for our cp parameter, using the 
#expand.grid function.
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
#This will define our cp parameters to test as numbers from 0.01 to 0.5, 
#in increments of 0.01.

#We can perform cross validation using the train function
#rpart since we want to cross validate a CART model and then trControl = numFolds,
#the output of our trainControl function, and then tuneGrid = cpGrid, the output 
# of the expand.grid function.
train(Reverse ~ Circuit + Issue + Petitioner + Respondent +
        LowerCourt + Unconst, data = Train, method = "rpart",
      trControl = numFolds, tuneGrid = cpGrid)
#The first column gives the cp parameter that was tested, and the second column 
#gives the cross validation accuracy for that cp value.The accuracy starts lower,
#and then increases, and then will start decreasing again
#At the end the cp value for the best accuracy is mentioned

#creating a new CART model with this cp value instead of the minbucket parameter
StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent +
                         LowerCourt + Unconst, data = Train, method = "class",
                       cp = 0.18)
#Let us make predictions
PredictCV <- predict(StevensTreeCV, newdata = Test, type = "class")
#Confusion Matrix to check accuracy
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)
#This is significantly better than the previous model with an accuracy of 0.659
#Cross  Validation helps us to select good parameter value which improve the 
#accuracy. If we had accidentally selected good parameter values then it 
#wont change much