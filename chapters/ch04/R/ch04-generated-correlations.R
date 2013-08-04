#install.packages("ecodist")
library(ecodist)

set.seed(1492)

x = seq.int(1:200)
y.pos = corgen(len=200,x,r=0.85)
y.none = corgen(len=200,x,r=0.0)
y.neg = corgen(len=200,x,r=-0.85)

par(mfrow=c(3,1))
plot(y.pos$x,y.pos$y,main="Positive Correlation",xlab="x",ylab="y")
plot(y.none$x,y.none$y,main="No Correlation",xlab="x",ylab="y")
plot(y.neg$x,y.neg$y,main="Negative Correlation",xlab="x",ylab="y")
par(mfrow=c(1,1))

#install.packages("TeachingDemos")
library(TeachingDemos)

par(mfrow=c(3,1))
cor.rect.plot(y.pos$x, y.pos$y, corr = FALSE,xlab="x",ylab="y")
cor.rect.plot(y.none$x, y.none$y, corr = FALSE,xlab="x",ylab="y")
cor.rect.plot(y.neg$x, y.neg$y, corr = FALSE,xlab="x",ylab="y")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
cor.rect.plot(y.pos$x, y.pos$y, corr = TRUE,xlab="x",ylab="y")
cor.rect.plot(y.none$x, y.none$y, corr = TRUE,xlab="x",ylab="y")
cor.rect.plot(y.neg$x, y.neg$y, corr = TRUE,xlab="x",ylab="y")
par(mfrow=c(1,1))

cov(y.pos$x, y.pos$y)
cov(y.none$x, y.none$y)
cov(y.neg$x, y.neg$y)

require(stats)
require(graphics)
summary(anscombe)

##-- now some "magic" to do the 4 regressions in a loop:
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

## See how close they are (numerically!)
sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)



