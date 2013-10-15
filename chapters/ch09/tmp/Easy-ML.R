library(ggplot2)

########################################
## This generated the data, do not share
set.seed(1492)
training <- data.frame(x=rnorm(53)+1, y=rnorm(53)+1, type="Infected")
training <- rbind(training, data.frame(x=rnorm(194)-1, y=rnorm(194)-1, type="Normal"))
hosts <- paste0("crisnd", sprintf("%04d", sample(9999, nrow(training))))
training <- cbind(system=hosts, training[sample(nrow(training)), ])
names(training) <- c("host", "proc", "mem", "state")
write.csv(training, "data/memproc.csv", row.names=F)
########################################

memproc <- read.csv("data/memproc.csv", header=T)
summary(memproc)

gg <- ggplot(memproc, aes(proc, mem, color=state))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point(size=3) + theme_bw()
print(gg)
ggsave("figures/793725c06f001.pdf", gg, width=8, height=5)

# make this repeatable
set.seed(1492)
# get how many in the overall sample
n <- nrow(memproc)
# set the test.size to be 1/3rd
test.size <- as.integer(n/3)
# randomly sample the rows for test set
testset <- sample(n, test.size)

# now split the data into test and train
test <- memproc[testset, ]
train <- memproc[-testset, ]

inf <- colMeans(train[train$state=="Infected", c("proc", "mem")])
nrm <- colMeans(train[train$state=="Normal", c("proc", "mem")])

print(inf)
##     proc      mem 
## 1.152025 1.201779 
print(nrm)
##       proc        mem 
## -0.8701412 -0.9386983 

predict.malware <- function(proc, mem) {
  # set up infected comparison
  inf.a <- inf['proc'] - proc
  inf.b <- inf['mem'] - mem
  # pythagorean distance c = sqrt(a^2 + b^2)
  inf.dist <- sqrt(inf.a^2 + inf.b^2)
  # repeat for normal systems
  nrm.a <- nrm['proc'] - proc
  nrm.b <- nrm['mem'] - mem
  nrm.dist <- sqrt(nrm.a^2 + nrm.b^2)
  # assign a label of the closest (smallest)
  ifelse(inf.dist<nrm.dist,"Infected", "Normal")
}

predict.malware(inf['proc'], inf['mem'])
predict.malware(nrm['proc'], nrm['mem'])

prediction <- apply(test, 1, function(row) {
  proc <- as.numeric(row[['proc']])
  mem <- as.numeric(row[['mem']])
  predict.malware(proc, mem)
})
sum(result$state==prediction)/nrow(result)
## [1] 0.8780488
result <- cbind(test, predict=prediction)
result$Accurate <- ifelse(result$state==result$predict, "Yes", "No")


slope <- -1*(1/((inf['mem']-nrm['mem'])/(inf['proc']-nrm['proc'])))
intercept <- mean(c(inf['mem'], nrm['mem'])) - (slope*mean(c(inf['proc'], nrm['proc'])))

result$Accurate <- factor(result$Accurate, levels=c("Yes", "No"), ordered=T)

gg <- ggplot(result, aes(proc, mem, color=state, size=Accurate, shape=Accurate))
gg <- gg + scale_shape_manual(values=c(16, 8))
gg <- gg + scale_size_manual(values=c(3, 6))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point() + theme_bw()
gg <- gg + geom_abline(intercept = intercept, slope = slope, color="gray80")
print(gg)
ggsave("figures/793725c06f002.pdf", gg, width=8, height=5)



print(nrm)
traina
##         x         y
## 1.0020913 0.9832088
trainb <- colMeans(training[training$type=="Normal", c("x", "y")])
trainb




# create the training data
training <- data.frame(x=rnorm(100)+1, y=rnorm(100, sd=2)+1, type="Infected")
training <- rbind(training, data.frame(x=rnorm(200)-1, y=rnorm(200, sd=2)-1, type="Normal"))

#create test data
test <- data.frame(x=rnorm(50)+1, y=rnorm(50, sd=2)+1, type="Infected")
test <- rbind(test, data.frame(x=rnorm(100)-1, y=rnorm(100, sd=2)-1, type="Normal"))

# plot it
gg <- ggplot(training, aes(x, y, color=type))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point(size=3) + theme_bw()
gg <- gg + xlab("Normalized Processor Usage") + ylab("Normalized Network Traffic")
print(gg)

# get the means of the two types
traina <- colMeans(training[training$type=="Infected", c("x", "y")])
traina
##         x         y
## 1.0020913 0.9832088
trainb <- colMeans(training[training$type=="Normal", c("x", "y")])
trainb
##          x          y 
## -0.8653709 -1.0049510 


prediction <- apply(test, 1, function(row) {
  x <- as.numeric(row[['x']])
  y <- as.numeric(row[['y']])
  dista <- sqrt((traina[1]-x)^2 + (traina[2]-y)^2)
  distb <- sqrt((trainb[1]-x)^2 + (trainb[2]-y)^2)
  ifelse(dista<distb,"Infected", "Normal")
})
head(prediction, 10)
##   1   2   3   4   5   6   7   8   9  10 
## "A" "A" "A" "A" "A" "B" "A" "A" "A" "B" 
sum(prediction[1:5]=="Infected")
## [1] 39
sum(prediction[6:55]=="Normal")
## [1] 47

slope <- -1*(1/((traina[2]-trainb[2])/(traina[1]-trainb[1])))
intercept <- mean(c(traina[2], trainb[2])) - (slope*mean(c(traina[1], trainb[1])))
hh <- gg + geom_abline(intercept = intercept, slope = slope)
print(hh)

test$accurate <- factor(ifelse(prediction==test$type, "Correct", "Incorrect"),
                        levels=c("Incorrect", "Correct"), ordered=T)

gg <- ggplot(test, aes(x, y, color=type, size=accurate, shape=accurate))
gg <- gg + scale_shape_manual(values=c(8, 16))
gg <- gg + scale_size_manual(values=c(6, 3))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point() + theme_bw()
gg <- gg + geom_abline(intercept = intercept, slope = slope)

print(gg)
# y






xseq <- seq(min(training$x), max(training$x), by=0.1)
yseq <- seq(min(training$y), max(training$y), by=0.1)
paint <- expand.grid(x=xseq, y=yseq)
paint$type <- apply(paint, 1, function(row) {
  x <- as.numeric(row[['x']])
  y <- as.numeric(row[['y']])
  dista <- sqrt((traina[1]-x)^2 + (traina[2]-y)^2)
  distb <- sqrt((trainb[1]-x)^2 + (trainb[2]-y)^2)
  ifelse(dista<distb,"A", "B")
})
gg <- ggplot(paint, aes(x, y, color=type))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point(size=3, alpha=1/5, shape=18) + theme_bw()
print(gg)


x1 <- xseq <- seq(min(training$x), max(training$x), by=0.1)
y1 <- slope*x1+intercept
areadf <- data.frame(x=x1, y=y1, type="B")

gg <- ggplot(training, aes(x, y, color=type))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point(size=3) + theme_bw()
gg <- gg + geom_abline(intercept = intercept, slope = slope)
gg <- gg + geom_area(data=areadf)
print(gg)


foo <- read.csv("data/vast/week1/PhysMem.csv", header=F)
foo <- read.csv("data/vast/week1/load.csv", header=F)
goo <- sapply(foo$V1, function(x) {
  as.numeric(sub('%', '', x))
})
