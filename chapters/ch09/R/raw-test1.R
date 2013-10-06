nf.srv <- read.csv("python/nf-chunk-ts.csv", header=T)
nf.srv <- read.csv("python/srv-1hr-ts.csv", header=T)
# srv-1hr-ts.csv
nf.o <- read.csv("python/nf-nosrv-ts.csv", header=T)
nf.srv <- read.csv("python/other-1hr-ts.csv", header=T)
#names(nf) <- c("ip", "ts", "sent", "recv")
#summary(nf$ts)
#hist(nf$ts)
#nf$realts <- nf$ts - min(nf$ts) + 1
#hist(nf$realts, breaks="scott")
# 
#print((max(nf$realts)/12)/24)
#summary(nf$ip, maxsum=10)

nf.srv$realts <- nf.srv$ts - min(nf.srv$ts) + 1
nf.o$realts <- nf.o$ts - min(nf.o$ts) + 1
fullts <- data.frame(realts=seq(max(nf.srv$realts)), realcount=0)

srv.sum <- aggregate(cbind(sent, recv) ~ realts, data=nf.srv, FUN=sum)
o.sum <- aggregate(cbind(sent, recv) ~ realts, data=nf.o, FUN=sum)

srv.merge <- merge(srv.sum, fullts, all=T)
srv.merge$sent[is.na(srv.merge$sent)] <- 0
srv.merge$recv[is.na(srv.merge$recv)] <- 0
o.merge <- merge(o.sum, fullts, all=T)
o.merge$sent[is.na(o.merge$sent)] <- 0
o.merge$recv[is.na(o.merge$recv)] <- 0

srv.sent <- ts(srv.merge$sent)
plot(srv.sent)
srv.recv <- ts(srv.merge$recv)
plot(srv.recv)
o.sent <- ts(o.merge$sent)
plot(o.sent)
o.recv <- ts(o.merge$recv)
plot(o.recv)

source("~/Documents/school/stat510/itall.R")



# and here's a regression example
-0.1678 + 0.0008313*6000000

## ML examples, simple and easy
# 
library(ggplot2)
set.seed(1492)
# create the training data
training <- data.frame(x=rnorm(100)+1, y=rnorm(100)+1, type="A")
training <- rbind(first.ml, data.frame(x=rnorm(100)-1, y=rnorm(100)-1, type="B"))

#create test data
test <- data.frame(x=rnorm(50)+1, y=rnorm(50)+1, type="A")
test <- rbind(test, data.frame(x=rnorm(50)-1, y=rnorm(50)-1, type="B"))

# plot it
ggplot(first.ml, aes(x, y, color=type)) + scale_color_brewer(palette="Set2") + 
  geom_point(size=3) + theme_bw()

# get the means of the two types
traina <- colMeans(training[training$type=="A", c("x", "y")])
traina
##         x         y
## 1.0020913 0.9832088
trainb <- colMeans(training[training$type=="B", c("x", "y")])
trainb
##          x          y 
## -0.8653709 -1.0049510 

prediction <- apply(test, 1, function(row) {
  x <- as.numeric(row[['x']])
  y <- as.numeric(row[['y']])
  dista <- sqrt((traina[1]-x)^2 + (traina[2]-y)^2)
  distb <- sqrt((trainb[1]-y)^2 + (trainb[2]-y)^2)
  ifelse(dista<distb,"A", "B")
})
head(prediction, 10)
##   1   2   3   4   5   6   7   8   9  10 
## "A" "A" "A" "A" "A" "B" "A" "A" "A" "B" 
sum(guess[1:50]=="A")
## [1] 39
sum(guess[51:100]=="B")
## [1] 47

