david <- read.csv("~/Documents/json/david/forbook5.csv")

# * Every IP is changed to a new value that is consistent throughout the file.
# * Client addresses (10.*.*.*) are converted to a random address in 10.1.*.*
# * Server addresses (172.*.*.*) are converted to a random address in 172.1.*.*
# * All other addresses (should be limited to 192.168.*.*) are converted to a random address in 192.168.*.*

library(zoo)
dav<-read.zoo("~/Documents/json/david/forbook2.csv", tz="", header=TRUE,format='%m-%d-%Y %H:%M:%S')
my.ts <- as.POSIXct(as.character(dav$timestamp), format='%m-%d-%Y %H:%M:%S')

david
david$date <- gsub("-", "/", david$date)
foo <- chron(david$date, david$time, format=c(dates="m/d/y", times="h:m:s"))
foo <- chron(as.character(david$date), david$time, format=c(dates="m/d/y", times="h:m:s"))


# forbook4 is with date/times correct
# forbook5 is with all zero-to-zero byte conversations removed

# preattentive:
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
theme_plain <- function() {
  theme(axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.border =  element_rect(fill=NA, colour = "gray50", size=0.5),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
    complete=TRUE)
}
theme_plain2 <- function() {
  theme(#axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.border =  element_blank(), # element_rect(fill=NA, colour = "black", size=1),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0.5,0,0,0), "lines"),
    complete=TRUE)
}

##
## Figure 6.1 and 6.2
##
# s.size <- 15^2
# set.seed(1490)
# x <- rep(seq(sqrt(s.size)), sqrt(s.size))
# y <- rep(seq(sqrt(s.size)), each=sqrt(s.size))
# color <- rep("gray50", s.size)
# color[7] <- "red"
# shape=rep(16, s.size)
# gdota <- data.frame(x, y, shape, color)
# dd <- ggplot(gdota, aes(x, y)) + geom_point(size=9, shape=shape, color=color) + theme_plain()
# print(dd)

# here is the easter egg
rawlabel <- paste("Writing a book is hard work, but one side perk is we get to inject easter eggs ",
                  "like this.  If you've found this, send us a message on twitter",
                  " (@hrbrmstr and @jayjacobs) saying \"Happy Easter\"!", sep="")
label <- paste("V3JpdGluZyBhIGJvb2sgaXMgaGFyZCB3b3JrLCBidXQgb25lIHNpZGUgcGVyayBpcyB3ZSBnZXQg",
               "dG8gaW5qZWN0IGVhc3RlciBlZ2dzIGxpa2UgdGhpcy4gIElmIHlvdSd2ZSBmb3VuZCB0aGlzLCBz",
               "ZW5kIHVzIGEgbWVzc2FnZSBvbiB0d2l0dGVyIChAaHJicm1zdHIgYW5kIEBqYXlqYWNvYnMpIHNh",
               "eWluZyAiSGFwcHkgRWFzdGVyIiE=", sep="")
label <- unlist(strsplit(label, NULL))
x <- rep(seq(length(label)/4), 4)
y <- rep(c(4:1), each=length(label)/4)
set.seed(1492)
x <- rnorm(length(x), mean=x, sd=0.02)
y <- rnorm(length(y), mean=y, sd=0.02)
#label[sample(s.size, 6)] <- c("C")
color <- rep("gray80", length(label))
color2 <- color
color2[which(label=="X")] <- "#AA0000"
shape=rep(16, length(label))
gdota <- data.frame(x, y, shape, color, color2, label=label)
first <- ggplot(gdota, aes(x, y, label=label)) + geom_text(size=8, color=color) + ylim(0.5, 4.5) + xlab("") + ylab("") + theme_plain2()
ggsave("figures/793725c06f001.pdf", plot=first, width=15, height=2)
second <- ggplot(gdota, aes(x, y, label=label)) + geom_text(size=8, color=color2) + ylim(0.5, 4.5) + xlab("") + ylab("") + theme_plain2()
ggsave("figures/793725c06f002.pdf", plot=second, width=15, height=2)

##
## Figure 6.3
##
s.size <- 16
set.seed(1486)
x <- rnorm(s.size, mean=seq(sqrt(s.size)), sd=0.2)
y <- rnorm(s.size, mean=rep(seq(sqrt(s.size)), each=sqrt(s.size)), sd=0.2)
shape <- rep(15, s.size)
shape[sample(seq(s.size), 3)] <- 16
shape2 <- ifelse(shape==16, 4, 21)
color <- ifelse(shape==16, "red", "#9999FF")
intense <- ifelse(shape==16, "black", "gray80")
sz <- ifelse(shape==16, 12, 6)
shape3 <- ifelse(shape==16, 15, 16)
gdata <- data.frame(x, y, shape, shape2, shape3, color, intense, sz)

gsize <- 6
aa <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, shape=shape2, color="black") + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Shape (a)") + theme_plain()
bb <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, shape=shape3, color="black") + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Shape (b)") + theme_plain()
cc <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, color=intense) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Intensity (d)") + theme_plain()
dd <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, color=color) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Hue (e)") + theme_plain()
ee <- ggplot(gdata, aes(x, y)) + geom_point(size=sz) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Size (c)") + theme_plain()
datums <- gdata[which(shape==16), ]
ff <- ggplot(gdata, aes(x, y)) + geom_point(size=5) + geom_point(data=datums, size=12, shape=21) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Enclosure (f)") + theme_plain()
#pdf("figures/793725c06f003.pdf", width=9, height=6)
grid.arrange(aa,bb,ee,cc,dd,ff, ncol=3, clip=T)
#dev.off()


##
## Figure 6.4
##
set.seed(3)
ksize <- 15
my.sd <- 0.6
x <- c(rnorm(ksize, mean=5, sd=my.sd), rnorm(ksize, mean=4, sd=my.sd))
y <- c(rnorm(ksize, mean=4, sd=my.sd), rnorm(ksize, mean=5, sd=my.sd))
clust <- rep(c("#FF0000", "#000066"), each=ksize)
shp <- rep(22, ksize*2)
shp[sample(seq(ksize*2), ksize)] <- 21
dfi <- data.frame(x, y, clust, shp)
sz <- ifelse(shp==22, 5, 4)
aa <- ggplot(dfi, aes(x,y)) + geom_point(size=sz, shape=shp, color="gray40", fill="gray80", show_guide=F) + 
  ggtitle("Shape (b)") + theme_plain()
#print(aa)
goo <- matrix(c(x,y), ncol=2)
cl <- kmeans(goo, 3)
# colors from colorbrewer: Set2
cls=factor(cl$cluster, labels=c("#8DA0CB" ,"#FC8D62", "#66C2A5"))
bb <- ggplot(dfi, aes(x,y)) + geom_point(size=4, shape=21, color="gray40", fill=as.character(cls)) + 
  ggtitle("Color (a)") + theme_plain()
#print(bb)
cc <- ggplot(dfi, aes(x,y)) + geom_point(size=sz, shape=shp, color="gray40", fill=as.character(cls), show_guide=F) + 
  ggtitle("Shape and Color (c)") + theme_plain()
#print(cc)
#pdf("figures/793725c06f004.pdf", width=9, height=3)
grid.arrange(bb,aa,cc, ncol=3, clip=T)
#dev.off()


set.seed(1492)
s.size=25
#x <- rnorm(s.size, mean=seq(sqrt(s.size)), sd=0.2)
#y <- rnorm(s.size, mean=rep(seq(sqrt(s.size)), each=sqrt(s.size)), sd=0.2)
ksize <- 20
my.sd <- 1
x <- c(rnorm(ksize, mean=6, sd=my.sd), rnorm(ksize, mean=4, sd=my.sd))
#y <- c(rnorm(ksize, mean=4, sd=0.5), rnorm(ksize, mean=6, sd=0.5))
y <- rnorm(ksize*2, mean=5)
clust <- rep(c("#CC0000", "#0000CC"), each=ksize)
shp <- rep(22, ksize*2)
shp[sample(seq(ksize*2), ksize)] <- 21
dfi <- data.frame(x, y, clust, shp)
ggplot(dfi, aes(x,y)) + geom_point(size=8, shape=shp, color="black", fill=clust, show_guide=F) + theme_plain()

y <- rnorm(500, mean=5)
goo <- matrix(c(x,y), ncol=2)
cl <- kmeans(goo, 3)
dfi <- data.frame(x=x, y=y, clust=as.factor(cl$cluster))
ggplot(dfi, aes(x,y,color=clust)) + geom_point()
centers <- data.frame(cl$centers)
centers$label <- seq(1,3)
names(centers) <- c("x", "y", "label")
gg <- plotDist2(dfi)
#  gg <- gg + geom_point(data=centers, aes(x, y, label), shape=3, color="black")
gg <- gg + geom_text(data=centers, aes(x, y, label=label), color="black")
print(gg)





shape <- rep(15, s.size)
shape[sample(seq(s.size), 3)] <- 16
shape2 <- ifelse(shape==16, 4, 21)
color <- ifelse(shape==16, "red", "#9999FF")
intense <- ifelse(shape==16, "black", "gray80")
sz <- ifelse(shape==16, 12, 6)
shape3 <- ifelse(shape==16, 15, 16)
gdata <- data.frame(x, y, shape, shape2, shape3, color, intense, sz)

gsize <- 6
aa <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, shape=shape2, color="black") + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Shape") + theme_plain()
bb <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, shape=shape3, color="black") + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Shape") + theme_plain()
cc <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, color=intense) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Intensity") + theme_plain()
dd <- ggplot(gdata, aes(x, y)) + geom_point(size=gsize, color=color) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Color (Hue)") + theme_plain()
ee <- ggplot(gdata, aes(x, y)) + geom_point(size=sz) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Size") + theme_plain()
datums <- gdata[which(shape==16), ]
ff <- ggplot(gdata, aes(x, y)) + geom_point(size=5) + geom_point(data=datums, size=12, shape=21) + 
  xlim(0.5, 4.5) + ylim(0.5,4.5) + ggtitle("Enclosure") + theme_plain()
pdf("figures/793725c06f003.pdf", width=9, height=6)
grid.arrange(aa,bb,ee,cc,dd,ff, ncol=3, clip=T)
dev.off()


