wine <- read.csv("wine.csv")
str(wine)
summary(wine)
model1 <- lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals ^ 2)
SSE
model2 <- lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals ^ 2)
SSE
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age 
             + FrancePop, data = wine)
summary(model3)
SSE = sum(model3$residuals ^ 2)
SSE
#Removing the insignificant variables
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + 
               Age, data = wine)
summary(model4)
#We removed only one variable at a time. At this step we can see than age is 
#significant. This is bcos of multicollinearity between age and FrancPop

#computing the Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
#To compute the correlation of the entire dataset. Use row column intersection
#to find the correlation between variable
cor(wine)

#If we remove both the variable together, we can see that the R-squared value
#drops significantly. So always remove variable one at a time

model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model5)

#Test data
wineTest <- read.csv("wine_test.csv")
str(wineTest)

#Prediction
predictTest <- predict(model4, newdata = wineTest)
predictTest

#Computing the R squared value of the test set
#Sum of squared errors
SSE <- sum((wineTest$Price - predictTest) ^ 2)
SST <- sum((wineTest$Price - mean(wine$Price)) ^ 2)
#R -squared value of the test set is 1-SSE/SST
1 - SSE/SST
