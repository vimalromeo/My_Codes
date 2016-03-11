library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
stevens <- read.csv("stevens.csv")
str(stevens)
#splitting the data to train and test
set.seed(3000)
spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)
Train <- subset(stevens, spl == TRUE)
Test <- subset(stevens, spl == FALSE)
#Building the CART model
#rpart(DepVar ~ IndVar1+ Indvar2+.., data =, method = "classification/regression,
#minbucket = )
#Minbucket is to limit the number of splits
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent +
                     LowerCourt + Unconst, data = Train, method = "class",
                     minbucket = 25)
#plotting the tree using prp function
prp(StevensTree)
#Making predictions. It uses t = 0.5 or majority class prediction if not specified
PredictCART <- predict(StevensTree, newdata = Test, type = "class")
#Computing the accuracy using confusion matrix
table(Test$Reverse, PredictCART)
#Accurcy
(41 + 71)/(41+36+22+71)
#Letz generate ROC Curve
PredictROC <- predict(StevensTree, newdata = Test)
PredictROC
#For each observation in the test set, it gives two numbers which can be thought
#of as the probability of outcome 0 and the probability of outcome 1.
#More concretely, each test set observationis classified into a subset, 
#or bucket, of our CART tree.

#We will use the second column as our probabilities to generate the ROC Curve
pred <- prediction(PredictROC[,2], Test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
