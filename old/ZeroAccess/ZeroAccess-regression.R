df <- read.csv("/home/jay/mac/zerogeo.csv", header=T)
# calculate how many people for 1 bot infection per state:
df$per <- round(df$intUsers/df$bots)
# plot histogram of the spread
hist(df$per, breaks=10, col="#CCCCFF", freq=T, main="Internet Users per Bot Infection")
# show summary of the spread
summary(df$per)
readline("Press <Enter> to continue")

# run linear regression on how internet users describes bot infections
# ----
users <- lm(df$bots~df$intUsers)
plot(df$intUsers, df$bots, xlab="Internet Users", ylab="Bots", pch=19, cex=0.7, col="#3333AA")
abline(users, col="#3333AA")
summary(users)
readline("Press <Enter> to continue")

# after running the above, plot out various regression plots:
# residuals versus fitted values, a Q-Q plot of standardized residuals, 
# a scale-location plot and a plot of residuals versus leverage 
# set up a 2 x 2 graphic:
par(mfrow=c(2,2)) 
plot(users)
# go back to normal for whatever graphic comes up next:
par(mfrow=c(1,1))