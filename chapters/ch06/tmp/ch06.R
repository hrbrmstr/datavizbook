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
library(gdata)

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
theme_plain3 <- function() {
  theme(#axis.title = element_blank(),
    #axis.text = element_blank(),
    panel.background = element_blank(),
    panel.border =  element_blank(), # element_rect(fill=NA, colour = "black", size=1),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.margin = unit(0.2, "cm"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(2,0.5,1,0.5), "lines"),
    complete=TRUE)
}
theme_plain4 <- function() {
  theme(#axis.title = element_blank(),
    #axis.text = element_blank(),
    panel.background = element_blank(),
    panel.border =  element_blank(), # element_rect(fill=NA, colour = "black", size=1),
    panel.grid = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.margin = unit(0.2, "cm"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0.5,0.5,1,0.5), "lines"),
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
#pdf("figures/793725c06f003.pdf", width=9, height=6)
grid.arrange(aa,bb,ee,cc,dd,ff, ncol=3, clip=T)
#dev.off()

##
##
## pie versus bar
##
##

myd <- c(24,22,20,18,16)
names(myd) <- c("A", "B", "C", "D", "E")
mycol <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")
#pie(myd, radius=1, clockwise=T, col=mycol)
mydf <- data.frame(count=myd*.01, label=names(myd))
aa <- ggplot(mydf, aes(x = "", y = count, fill = label)) + 
  geom_bar(width = 1, stat="identity", show_guide=F) + 
  scale_fill_manual(values = mycol) + xlab("") + ylab("") +
  geom_text(aes(x=c(1.3), y=c(.12, .35, .56, .74, .92), label = label)) +
  coord_polar("y", start=0) + theme_plain2()

bb <- ggplot(mydf, aes(x=label, y=count)) + geom_bar(fill=mycol, stat="identity") +
  ylab("") + xlab("") + theme_plain4() +
  scale_y_continuous(expand = c(0, 0), limits=c(0,.3))

pdf("figures/793725c06f006.pdf", width=10, height=4)
grid.arrange(aa, bb, ncol=2, clip=T)
dev.off()

library(binom)
mydf2 <- binom.confint(myd*3, 100*3, method="wilson")
mydf2$label <- names(myd)
mydf2$count <- mydf2$x*.01
cc <- bb + geom_errorbar(data=mydf2, aes(x=label, ymin=lower, ymax=upper), size=0.5, width=0.15, color="gray30") +
  geom_point(data=mydf2, aes(x=label, y=mean), color="black", fill="white", size=4, shape=23, show_guide=F) +
  theme_plain4()
pdf("figures/793725c06f006-alt.pdf", width=10, height=4)
grid.arrange(aa, cc, ncol=2, clip=T)
dev.off()

# what if we had a margin of error?
cc <- bb + geom_errorbar(aes(x=label, ymin=count-0.03, ymax=count+0.03), size=0.5, width=0.15, color="gray30") +
  geom_point(color="black", fill="white", size=4, shape=23, show_guide=F) +
  theme_plain4()
grid.arrange(aa, cc, ncol=2, clip=T)



## simple scatter plot
fw <- read.csv("~/Documents/book/bobfw/fivemin.csv", header=T)
# simple
gg <- ggplot(fw, aes(x=packet, y=bytes)) + geom_point()
print(gg)
ggsave("figures/793725c06f007.pdf", gg, width=8, height=5)

scale.filter <- function(x) {
  x[is.na(x)] <- 0
  humanReadable(x)
}
print(x)
  # we can't take a log of zero
  mult <- ifelse(x==0, 0, floor(log10(x)/3))
  print(mult)
  # create size labels
  mods <- c("k", "m", "g", "t")
  # don't modify less than k, and make it pretty
  ifelse(mult>0, paste(round(x/(10^(mult*3))), mods[mult], sep=""), 0)
}

gg <- ggplot(fw, aes(packet, bytes)) + geom_point()
gg <- gg + xlab("Packets") + ylab("Bytes") + theme_bw()
gg <- gg + ggtitle("Five Minutes Periods of Firewall Traffic")
gg <- gg + scale_x_continuous(labels=scale.filter)
gg <- gg + scale_y_continuous(labels=scale.filter)
print(gg)
ggsave("figures/793725c06f008.pdf", gg, width=8, height=5)

## simple line plot
fw <- read.csv("~/Documents/book/bobfw/fivemin-cats.csv", header=T)
# simple
fwspec <- fw[-which(fw$type=="All"), ]
gcolor[seq(1,nrow(bob), by=12)] <- "gray60"
pcolor=rep("gray60", nrow(bob))
pcolor[seq(1,nrow(bob), by=6)] <- "#CC0000"
mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
myb <- seq(1, length(unique(allfw$realx)), by=12)
better <- data.frame(hour=unique(fw$hour), realx=seq_along(unique(fw$hour)))
allfw <- merge(fw, better)
fwspec <- allfw[-which(allfw$type=="All"), ]

gg <- ggplot(fwspec, aes(realx, bytes, group=type, color=type))
gg <- gg + geom_smooth(stat="identity", fill="white") + theme_bw() + scale_y_log10(labels=scale.filter)
gg <- gg + scale_x_continuous(breaks=myb, label=mylabel)
gg <- gg + xlab("Time") + ylab("Bytes")
gg <- gg + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major.y=element_line(color="gray60"),
                 panel.grid.major.x=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks.length = unit(0, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(colour = '#FFFFFF00', fill = '#FFFFFF', size = 0.4))
print(gg)
ggsave("figures/793725c06f009.pdf", gg, width=8, height=5)

fwspec$bperp <- fwspec$bytes/fwspec$packets
fwagg <- aggregate(bperp ~ type, data=fwspec, FUN=mean)
fwagg$type <- factor(fwagg$type, levels=c("Workstation", "Server", "Printer", "Network"), ordered=T)
fwagg$bperp <- round(fwagg$bperp, 0)
gg <- ggplot(fwagg, aes(type, bperp, fill=type, label=bperp))
gg <- gg + geom_bar(stat="identity", show_guide=F)
gg <- gg + xlab("") + ylab("Bytes") + scale_fill_brewer(palette="Set2")
gg <- gg + geom_text(aes(x=fwagg$type, y=fwagg$bperp+15), size=4)
gg <- gg + ggtitle("Bytes per Packet by Device")
gg <- gg + theme_bw()
gg <- gg + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.background=element_blank(),
                 panel.grid.major.y=element_line(color="gray70"), 
                 panel.grid.major.x=element_blank(),
                 panel.grid.minor=element_blank(),
                 # axis.text.y = element_blank(),
                 axis.ticks.length = unit(0, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"))
print(gg)
ggsave("figures/793725c06f010.pdf", gg, width=8, height=5)

##
## Shapes
##
gg <- ggplot(fwspec, aes(packets, bytes, shape=type, color=type))
gg <- gg + scale_color_brewer(palette="Set2") + xlab("Packets") + ylab("Bytes")
gg <- gg + geom_point(size=5, alpha=1/3) + theme_bw()
gg <- gg + scale_x_log10(labels=scale.filter)
gg <- gg + scale_y_log10(labels=scale.filter) 
gg <- gg + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major.y=element_line(color="gray60"),
                 panel.grid.major.x=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks.length = unit(0, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(colour = '#FFFFFF00', fill = '#FFFFFF', size = 0.4))
#print(gg)
aa <- gg
gg <- ggplot(fwspec, aes(sessions, bytes, shape=type, color=type))
gg <- gg + scale_color_brewer(palette="Set2") + xlab("Sessions") + ylab("Bytes")
gg <- gg + geom_point(size=5, alpha=1/3) + theme_bw()
gg <- gg + scale_x_log10(labels=scale.filter)
gg <- gg + scale_y_log10(labels=scale.filter) 
gg <- gg + theme(legend.title=element_blank(),
                 panel.border=element_blank(),
                 panel.grid.major.y=element_line(color="gray60"),
                 panel.grid.major.x=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks.length = unit(0, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(colour = '#FFFFFF00', fill = '#FFFFFF', size = 0.4))
#print(gg)
grid.arrange(aa, gg, ncol=2, clip=T)

##
## size
##
wk <- fwspec[which(fwspec$type=="Network"), ]  # not log
gg <- ggplot(wk, aes(sessions, bytes, size=packets, color=type, fill=type))
gg <- gg + scale_color_brewer(palette="Set2") + xlab("Sessions") + ylab("Bytes")
gg <- gg + geom_point(alpha=1/3, shape=21, color="black", guide=F) + theme_bw()
gg <- gg + scale_size_continuous(range = c(2, 20), trans=log10_trans(), guide=F)
#gg <- gg + scale_x_log10(labels=scale.filter)
#gg <- gg + scale_y_log10(labels=scale.filter) 
gg <- gg + scale_x_continuous(labels=scale.filter)
gg <- gg + scale_y_continuous(labels=scale.filter) 
gg <- gg + theme(legend.position="none",
                 panel.border=element_blank(),
                 panel.grid.major.y=element_line(color="gray60"),
                 panel.grid.major.x=element_line(color="gray80"),
                 panel.grid.minor=element_blank(),
                 axis.ticks.length = unit(0, "cm"),
                 axis.ticks.margin = unit(0.1, "cm"),
                 legend.background = element_rect(colour = '#FFFFFF00', 
                                                  fill = '#FFFFFF', size = 0.4))
print(gg)
## OR

ggplot(bob, aes(iter, packet)) + geom_bar(stat="identity") + theme_bw()
bob$mins <- rep(seq(1,12), 8)
ggplot(bob, aes(iter, packet, fill=mins)) + geom_bar(stat="identity") + theme_bw()

# ML for bob
bob <- read.csv("~/Documents/book/bobfw/lm-int.csv", header=T)
cnames <- colnames(bob[ ,-1])
outf <- apply(bob[ ,-1], 2, function(x) ifelse(x==0, 0, log10(sqrt(x))))
outp <- apply(bob[ ,-1], 2, function(x) ifelse(x>=mean(x), 1, 0))
outm <- apply(outf, 2, function(x) x/max(x))
other <- as.matrix(outm)

other <- as.matrix(bob[ ,-1])
rownames(other) <- bob[ ,1]

inmat <- other %*% t(other)
# almost 8 minutes on this next one
system.time(d <- dist(other))
system.time(d <- dist(inmat))
system.time(fit <- cmdscale(d, eig=TRUE, k=2))
x <- fit$points[,1]
y <- fit$points[,2]
dfi <- data.frame(x=x, y=y, label=row.names(other))
ggplot(dfi, aes(x,y)) + geom_point(alpha=1/10)
ggplot(dfi, aes(x,y)) + geom_point(size=8, alpha=1/15) + theme_plain2() + xlab("") + ylab("")

ggplot(dfi, aes(x,y)) + geom_point(size=2, alpha=1/3) + scale_y_log10() + scale_x_log10() + theme_plain2()
ggplot(dfi, aes(x,y)) + geom_point() + xlim(min(x), 0) + 
  ylim(-400000000000000, 1000000000000000)

print(plotDist2(dfi))

# simple stats file
bob <- read.csv("~/Documents/book/bobfw/hourly.csv", header=T)
mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
bob$label <- factor(mylabel, levels=mylabel, ordered=T)
ggplot(bob, aes(label, bytes)) + geom_bar(stat="identity") + theme_bw()
ggplot(bob, aes(bytes,packet)) + geom_point() + scale_y_log2() + scale_x_log2() + theme_bw()
ggplot(bob, aes(count,packet)) + geom_point() + theme_bw()
ggplot(bob, aes(count,bytes)) + geom_point() + theme_bw() 
megformat <- function(x) paste(round(x/1000000), "m", sep="")
gigformat <- function(x) paste(round(x/1000000000, 1), "g", sep="")

ggplot(bob, aes(bytes,packet)) + geom_point() + theme_bw() + xlab("Bytes") + ylab("Packets") +
  scale_y_continuous(labels=megformat) + scale_x_continuous(labels=gigformat)

scale_x_continuous(#trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))
  scale_y_log10(labels=trans_format('log10',math_format(10^.x))) + scale_x_log10() + theme_bw()
)

bob <- read.csv("~/Documents/book/bobfw/fivemin.csv", header=T)
bob <- bob[order(bob$hour), ]
bob$iter <- seq_along(bob$hour)
ggplot(bob, aes(iter, packet)) + geom_bar(stat="identity") + theme_bw()
bob$mins <- rep(seq(1,12), 8)
ggplot(bob, aes(iter, packet, fill=mins)) + geom_bar(stat="identity") + theme_bw()

# rolling average (2)
x <- seq(2, nrow(bob))
y <- sapply(x, function(z) sum(bob$count[(z-1):z])/2)
dfi <- data.frame(x, y)
gcolor=rep("gray95", nrow(dfi))
gcolor[seq(1,nrow(dfi), by=12)] <- "gray60"
pcolor=rep("gray60", nrow(dfi))
pcolor[seq(1,nrow(dfi), by=6)] <- "#CC0000"
ggplot(dfi, aes(x, y)) + geom_bar(stat="identity", width=0.1, color=gcolor) + 
  geom_point(size=2, color=pcolor) + theme_bw() + ylab("Sessions") + xlab("") +
  scale_x_continuous(labels=mylabel, breaks=seq(2,nrow(dfi), by=12)) +
  scale_y_continuous(labels=c("0", "100k", "200k", "300k", "400k"))
# bytes
  #scale_y_continuous(labels=c("0", "500m", "1g", "oh"))
  # count

gcolor=rep("gray97", nrow(bob))
gcolor[seq(1,nrow(bob), by=12)] <- "gray60"
pcolor=rep("gray60", nrow(bob))
pcolor[seq(1,nrow(bob), by=6)] <- "#CC0000"

ggplot(bob, aes(iter, count)) + geom_bar(stat="identity", width=0.1, color=gcolor) + 
  geom_point(size=3, color=pcolor) + theme_bw() + ylab("Sessions") + xlab("") +
  scale_x_continuous(labels=mylabel, breaks=seq(1,nrow(dfi), by=12)) +
  scale_y_continuous(labels=c("0", "100k", "200k", "300k", "400k"))


##
## line chart
##
bob <- read.csv("~/Documents/book/bobfw/fivemin.csv", header=T)
zero.scale <- function(x) {
  (x-min(x))/max(x)
}
foo <- data.frame(label="packet", y=bob$packet, yscale=zero.scale(bob$packet))
foo <- rbind(foo, data.frame(label="bytes", y=bob$bytes, yscale=zero.scale(bob$bytes)))
foo <- rbind(foo, data.frame(label="count", y=bob$count, yscale=zero.scale(bob$count)))
foo$x <- rep(seq(96), 3)

ggplot(foo, aes(x, y, group=label, color=label)) + geom_line() + theme_bw() + scale_y_log10()

ggplot(foo, aes(x, yscale, group=label, color=label)) + geom_line() + geom_point() + theme_bw()