mfrow <- par()$mfrow
mar <- par()$mar
oma <- par()$oma

par(mfrow=c(3, 2), mar=c(0, 0, 2, 0), oma=c(1, 1, 1, 1))
set.seed(2)
x <- runif(500, min=0, max=pi)

y <- rnorm(500, mean=x, sd=0.2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=x, sd=2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=-x, sd=0.3)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=-x, sd=1)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=sin(x), sd=0.2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- runif(500, min=0, max=pi)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

par(mfrow=mfrow, mar=mar, oma=oma)
