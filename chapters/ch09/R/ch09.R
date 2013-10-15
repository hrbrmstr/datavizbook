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
#ggsave("figures/793725c09f001.pdf", gg, width=8, height=5)

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

# could test with these if you uncomment them
# predict.malware(inf['proc'], inf['mem'])
# expected "Infected" 
# predict.malware(nrm['proc'], nrm['mem'])
# expected "Normal"

prediction <- apply(test, 1, function(row) {
  proc <- as.numeric(row[['proc']])
  mem <- as.numeric(row[['mem']])
  predict.malware(proc, mem)
})

sum(test$state==prediction)/nrow(test)
## [1] 0.8780488


### The following will generate figure 9.2
## will be on website.
slope <- -1*(1/((inf['mem']-nrm['mem'])/(inf['proc']-nrm['proc'])))
intercept <- mean(c(inf['mem'], nrm['mem'])) - (slope*mean(c(inf['proc'], nrm['proc'])))

result <- cbind(test, predict=prediction)
result$Accurate <- ifelse(result$state==result$predict, "Yes", "No")
result$Accurate <- factor(result$Accurate, levels=c("Yes", "No"), ordered=T)

gg <- ggplot(result, aes(proc, mem, color=state, size=Accurate, shape=Accurate))
gg <- gg + scale_shape_manual(values=c(16, 8))
gg <- gg + scale_size_manual(values=c(3, 6))
gg <- gg + scale_color_brewer(palette="Set2")
gg <- gg + geom_point() + theme_bw()
gg <- gg + geom_abline(intercept = intercept, slope = slope, color="gray80")
print(gg)
#ggsave("figures/793725c09f002.pdf", gg, width=8, height=5)

### end figure 9.2


### This will generate figure 9.3, Not in book but on website
set.seed(1)
x <- runif(200, min=-10, max=10)
y <- 1.377*(x^3) + 0.92*(x^2) + .3*x + rnorm(200, sd=250) + 1572
x <- x + 10
smooth <- ggplot(data.frame(x,y), aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se=F) + 
  theme_bw()
print(smooth)
#ggsave("figures/793725c09f003.eps", smooth, width=7, height=5)

########## logistic for plot 9.4
memproc <- read.csv("data/memproc.csv", header=T)
memproc$infected <- ifelse(memproc$state=="Infected", 1, 0)
set.seed(1492)
n <- nrow(memproc)
test.size <- as.integer(n/3)
testset <- sample(n, test.size)
test <- memproc[testset, ]
train <- memproc[-testset, ]

glm.out = glm(infected ~ proc + mem, data=test, family=binomial(logit))
summary(glm.out)
modelog <- predict.glm(glm.out, test, type="response")
gg <- ggplot(data.frame(x=modelog, y=ifelse(test$infected>0.5, "Yes", "No")), aes(x, y)) +
  geom_point(size=3, fill="steelblue", color="black", shape=4) + 
  ylab("Known Infected Host") +
  xlab("Estimated Probability of Infected Host") + theme_bw()
print(gg)
#ggsave("figures/793725c09f004.eps", gg, width=7, height=2)
# estimate where?  0.4 for now.
goo <- ifelse(foo>0.4, 1, 0)
sum(goo==test$infected)/length(goo)

##### K-means, figure 9.5 (again, just for website)
library(gridExtra)
set.seed(1)
x <- c(rnorm(200), rnorm(400)+2, rnorm(400)-2)
y <- c(rnorm(200), rnorm(200)+2, rnorm(200)-2, rnorm(200)+2, rnorm(200)-2)
randata <- data.frame(x=x, y=y)
out <- list()
for(i in c(3,4,5,6)) {
  km <- kmeans(randata, i)
  centers <- data.frame(x=km$centers[ ,1], y=km$centers[ ,2], cluster=1)
  randata$cluster <- factor(km$cluster)
  gg <- ggplot(randata, aes(x, y, color=cluster)) + geom_point(size=2)
  gg <- gg + geom_point(data=centers, aes(x, y), shape=8, color="black", size=4)
  gg <- gg + scale_x_continuous(expand=c(0,0.1))
  gg <- gg + scale_y_continuous(expand=c(0,0.1))
  gg <- gg + ggtitle(paste("k-means with", i, "clusters"))
  gg <- gg + theme(panel.grid = element_blank(),
                   panel.background = element_rect(colour = "black", fill=NA),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   legend.position = "none",
                   axis.ticks = element_blank())
  out[[i-2]] <- gg
}
print(out[[1]])
print(out[[2]])
print(out[[3]])
print(out[[4]])
# setEPS()
# postscript(file="figures/793725c09f005.eps", paper="special", 
#            width=8, height=6, horizontal=FALSE) 
# grid.arrange(out[[1]], out[[2]], out[[3]], out[[4]], ncol=2, clip=T)
# dev.off()



#### MDS and Hclust with VCDB data
# if you haven't installed verisr yet, do these two steps:
library(devtools)
# now let's install the verisr package
install_github("verisr", "jayjacobs")

# and load up the library
library(verisr)
library(ggplot2)

# grab the incidents from the VCDB repository
# https://github.com/vz-risk/VCDB
# set the dir to the incidents/ directly of that
jsondir <- '../VCDB/incidents'
#jsondir <- '../../../VCDB/incidents'
# create a veris instance with the vcdb data
vcdb <- json2veris(jsondir)

# convert veris object into a numeric matrix
vmat <- veris2matrix(vcdb)

# now pull the column names and extract industries
vmat.names <- colnames(vmat)
industry <- vmat.names[grep('victim.industry', vmat.names)]

# "fold" the matrix on industries
# this pulls all the incidents for the industry
# and compresses so the proportions of the features are represented.
imat <- foldmatrix(vmat, industry, min=10, clean=T)

#rownames(imat) <- txt.label

#idist <- dist(imat, 'canberra')
#pca <- prcomp(idist) #, scale=T, center=T)
# NO pca <- prcomp(imat, scale=T, center=T)
# create ggplot data.frame
#df <- data.frame(x=pca$x[ ,1], y=pca$x[ ,2], label=ind.label, size=ind.counts)

# convert the distance matrix
idist <- dist(imat, method='canberra')
# run it through classical MDS
cmd <- cmdscale(idist)
#plot(cmd)
# get the size of bubbles
ind.counts <- colSums(vmat[ , rownames(cmd)])
# extract the industry label
ind.label <- sapply(rownames(cmd), function(x) { 
  tail(unlist(strsplit(x, "[.]")), 1) 
})
# load up industry data
data(industry2)
# create a new list of short tet
txt.label <- industry2$short[which(industry2$code %in% ind.label)]


indf <- data.frame(x=cmd[ ,1], y=cmd[, 2], label=txt.label, size=ind.counts)
library(ggplot2)
gg <- ggplot(indf, aes(x, y, label=label, size=size))
gg <- gg + scale_size(trans="log2", range=c(10,30), guide=F)
gg <- gg + geom_point(fill="lightsteelblue", color="white", shape=21)
gg <- gg + xlim(range(df$x)*1.1) # expand x scale
gg <- gg + geom_text(size=4)
gg <- gg + theme(panel.grid = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank())
print(gg)
#ggsave("figures/793725c09f006.eps", gg, width=8, height=5)

# go back and relabel imat
rownames(imat) <- txt.label
# rerun idist
idist <- dist(imat, 'canberra')
# hclust couldn't be easier
hc <- hclust(idist) # , method="complete")
plot(hc)
# setEPS()
# postscript(file="figures/793725c09f007.eps", paper="special", 
#            width=8, height=5, horizontal=FALSE) 
# plot(hc)
# dev.off()

# we can now cut off the heirarchical clustering at some level
# and use those levels to color the MDS plot
indf$cluster <- as.factor(cutree(hc, 5))
gg <- ggplot(indf, aes(x, y, label=label, size=size, fill=cluster))
gg <- gg + scale_size(trans="log2", range=c(10,30), guide=F)
gg <- gg + geom_point(color="gray80", shape=21)
gg <- gg + xlim(range(df$x)*1.04) # expand x scale
gg <- gg + geom_text(size=4)
gg <- gg + theme(panel.grid = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 legend.position="none",
                 axis.ticks = element_blank())

print(gg)
#ggsave("figures/793725c09f008.eps", gg, width=8, height=5)
