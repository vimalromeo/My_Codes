baseball <- read.csv("baseball.csv")
str(baseball)

#Subsetting the data for taking only years before 2002
moneyball <- subset(baseball, Year < 2002)
str(moneyball)

#We need to build an LM model for predicting the wins based on the difference
#between the runs scored and the runs allowed

#Run Diff = Runs Scored - Runs Allowed
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)

#Creating a scatterplot to visualize the relationship btwn wins(W) and RD
plot(moneyball$RD, moneyball$W)

#Creating an LM model
WinReg <- lm(W ~ RD, data = moneyball)
summary(WinReg)

# W = 80.8814+0.1058*RD
# W >= 95 for playoffs (From plotting the teams and runs)
# 80.8814+0.1058*RD >= 95
# RD >= 133.4
# This is very close to the claim made in moneyball that a team needs to score
# 135 runs more than they allow to qualify for the playoffs

#LM model for predicting runs scored
str(moneyball)
#Auckland A's took on-base percentage(OBP) and slugging percentage(SLG) as the 
#2 most imp factors for determining the runs a player scored. Other teams focused
#on batting avg(BA)

RunsReg <- lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)

#BA has a -ve relationship with Runs scored. This is counter intuitive. This is
#because of multi collinearity. Let us remove BA and check

RunsReg <- lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)

#Findings proves the claims made in moneyball
# *Batting Avg is overvalued
# * On-base percentage is the most valued(comparison is fine as OBP and SLG are
#   in the same units-percents )
# * Slugging Percentage is also important

#We can create an LM model for runs allowed or runs scored by the opponent using
#Opponent OBP and Opponent SLG

AllwdReg <- lm(RA ~ OOBP + OSLG, data = moneyball)
summary(AllwdReg)
