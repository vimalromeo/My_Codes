BigMart <- read.table("/home/romeo/Desktop/analytics_vidhya/BigMart Sales/Train_UWu5bXk.csv",
                      header = T, sep = ",")
###
head(BigMart)
###structure
str(BigMart)
#histogram
hist(BigMart$Item_Outlet_Sales)
#sales summary
summary(BigMart$Item_Outlet_Sales)
###
summary(BigMart)
mean(BigMart$Item_Weight)
#Getting Mean
MeanItem_Weight <- mean(BigMart$Item_Weight[!is.na(BigMart$Item_Weight)])
#Replacing
BigMart$Item_Weight[is.na(BigMart$Item_Weight)] <- MeanItem_Weight
###Converting to character type for replacing string anomalies
BigMart$Item_Fat_Content <- as.character(BigMart$Item_Fat_Content)
BigMart$Item_Fat_Content <- gsub("LF", "lowfat",BigMart$Item_Fat_Content)
BigMart$Item_Fat_Content <- gsub("low fat", "lowfat",BigMart$Item_Fat_Content)
BigMart$Item_Fat_Content <- gsub("Low Fat", "lowfat",BigMart$Item_Fat_Content)
BigMart$Item_Fat_Content <- gsub("reg", "Regular",BigMart$Item_Fat_Content)
#Converting back to factors
BigMart$Item_Fat_Content <- as.factor(BigMart$Item_Fat_Content)
summary(BigMart)
