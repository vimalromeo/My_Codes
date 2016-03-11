library(caTools)
library(ROCR)
quality <- read.csv("quality.csv")
str(quality)
#checking the number of patients who got good care and poor care
#It shows 98 patients got good care-coded 0 and 33 got poor care-coded 1
table(quality$PoorCare)
#Splitting the data into training and test using caTools package
#set seed for having the same sample as the online course
set.seed(88)
#The function used is sample.split(). It has 2 arguments. First is the outcome 
#variable and the second is the split ratio.
#This function makes sure the outcome variable is well balanced in both the splits
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
#TRUE means we should put that observation in the training set. FALSE means we 
#should put it in the test set
split
#creating the splits
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

#We will build a logistic regression model with OfficeVisits and Narcotics as
#independent variables
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, 
                  data = qualityTrain, family = binomial)
summary(QualityLog)

#Prediction. This tell the predict function to give probabilities
predictTrain <- predict(QualityLog, type = "response")
#The values should be between 0 and 1
summary(predictTrain)
#To check whether we are predicting higher probabilities for actual Poor Cases
#This will compute the avg prediction for each true outcome
tapply(predictTrain, qualityTrain$PoorCare,mean)
#We predict an avg prob of abt 0.44. For all true good care cases we predict an 
#avg prob of 0.19. This is good bcos we are prediciting a higher prob for
#actual poor care cases

#Creating a confusion matrix
#Using a threshold value of 0.5. We should label the rows by true outcome
table(qualityTrain$PoorCare, predictTrain > 0.5)
#From the table, we predict 70 cases as good care and it is actually good care
#we predict 10 cases as poor care and it is actually poor care.
#We have predicted 15 cases as good care wrongly and 4 cases as poor care wrongly

#Calculating Sensitivity and specificity
#Sensitivity = 10/(10+15)
10/25
#Specificity = 70/(70+4)
70/74
#increasing the threshold value to 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
#Sensitivity
8/25
#Specificity
73/74
#increasing the threshold value to 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)
#Sensitivity
16/25
#Specificity
54/74

#Creating the ROC Curve
#We will use the predictions made on our training set to create the ROC Curve
#We will call the prediction function of ROCR. We will name the output as ROCRPred
#The function takes 2 args. The first is the prediction made with our model
#the second is the true outcome of the data points
ROCRPred <- prediction(predictTrain, qualityTrain$PoorCare)
#Now we use the performance fn.It defines what we plot on x & y axis of ROC curve
#It takes the output of prediction fn and what we want on x and y axis as inputs
ROCRperf <- performance(ROCRPred, "tpr", "fpr") #true positive rate & false positive rate
plot(ROCRperf)
#To add colors
plot(ROCRperf, colorize = TRUE)
#To add threshold labels to our plot btwn 0 and 1 at increments of 0.1
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1) ,
     text.adj = c(-0.2, 1.7) )

#Out of sample prediction
predictTest <- predict(QualityLog, type = "response", 
                       newdata = qualityTest)
#Using a threshold value of 0.3
table(qualityTest$PoorCare, predictTest > 0.3)
#Out of sample Accuracy
25/32
