zero <- read.csv("/home/jay/mac/zeroaccess/zip-pop-inc.csv", header=T)

inc <- rep(zero$MedianIncome, zero$BotCount)
pop <- rep(zero$Population, zero$BotCount)
plot(inc, pop, cex=.3, pch=19, col="#FF000011")
i30 <- table(cut(inc, 30))
p30 <- table(cut(pop, 30))
NameConv <- function(x) {
  temp <- c(substring(x[1], 2), substr(x[2], 1, nchar(x[2])-1))
  mean(type.convert(temp))
}
names(i30) <- sapply(strsplit(names(i30), ','), NameConv)
names(p30) <- sapply(strsplit(names(p30), ','), NameConv)

botcount <- tapply(zero$BotCount, zero$IncomeRounded, sum)
popcount <- tapply(zero$PopulationRoundes, zero$IncomeRounded, sum)
y <- as.vector(botcount) # bot counts
x1 <- type.convert(names(botcount)) # income levels
x2 <- as.vector(popcount)

fit <- lm(y ~ x1 + x2 + x1:x2)
fit <- lm(y ~ x1 + x2)
fit <- lm(y ~ x1:x2)
library(effects)
#model.lm <- lm(formula=y ~ x*f,data=thedata)
plot(effect(term="x2",mod=fit,default.levels=8),multiline=TRUE)


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

zr <- data.frame(pop = popcount, bot = botcount, 
                 label=sapply(names(botcount),
                              function(x) { paste(as.numeric(x)/1000, "k", sep="") }))
ggplot(zr, aes(pop, bot)) +
  ggtitle("Bot Infections vs. Population (by income)") +
  geom_point(size=3, colour="#000066", alpha=I(1/2)) +
  ylab("Bot Infections") +
  geom_smooth(method=lm) + 
  scale_x_continuous("Population by income", labels=
                 function(x) { paste(as.numeric(x)/1000000, "M", sep="") }) +
 geom_text(data=zr[zr$bot>900,], aes(pop, bot, label = label), hjust = 0.5, vjust=1.5) + 
  theme_bw()
