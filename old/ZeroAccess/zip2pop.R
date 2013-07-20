zero <- read.csv("~/Dropbox/ZeroAccess/unfiltered-zip-pop-inc.csv", header=T)

botcount <- tapply(zero$BotCount, zero$IncomeRounded, sum)
popcount <- tapply(zero$PopulationRoundes, zero$IncomeRounded, sum)

#botcount = aggregate(zero$BotCount,by=list(Category=zero$PopulationRoundes),FUN=sum)
#popcount = aggregate(zero$PopulationRoundes,by=list(Category=zero$IncomeRounded),FUN=sum)

#  Compare how population and botcount compare by median Income
# ----
par(mfrow=c(2,1)) 
barplot(botcount, main="Bot Infections by Median Income")
barplot(popcount, main="Populations by Median Income")
par(mfrow=c(1,1))

# Run linear regression, see how population is related to bot count
# ----
poor <- lm(botcount~popcount)
summary(poor)
print(anova(poor))

# Plotting out the relation between the two variables with regression line
# ----
plot(popcount, botcount, cex=.6, pch=19, frame.plot=F,
     xlab="Population", ylab="Bot Infections", col="#FF0000",
     main="Bot Infections vs. Population by Median Income")
abline(poor)
# label the points, it's nice to see what's where
text(popcount, botcount, names(popcount), pos=3, cex=.7)

# show the regression graphics
# ----
# it looks like the variance may not be consistent
# room for improvement on the linear equation
par(mfrow=c(2,2)) 
plot(poor)
# go back to normal for whatever graphic comes up next:
par(mfrow=c(1,1))

# now visually compare the expected value from regression
# ----
# applying linear regression on this data set:
# bot infections = -20.25 + 0.00006428*Population 
# values are held in fitted value in lm return value
# estimations can't be < 0 in reality
expected <- ifelse(poor$fitted.values < 0, 0, poor$fitted.values)
print(botTable)
par(mfrow=c(3,1)) 
barplot(botcount, main="Bot Infections by Median Income")
barplot(popcount, main="Populations by Median Income")
barplot(expected, main="Expected: BotCount = -20.25 + 0.00006428*Population")
par(mfrow=c(1,1))

