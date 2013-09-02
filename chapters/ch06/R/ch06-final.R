library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gdata)

# load these functions:
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
scale.filter <- function(x) {
  x[is.na(x)] <- 0
  humanReadable(x)
}
scale.filter.nb <- function(x) {
  x[is.na(x)] <- 0
  sub("B", "", humanReadable(x))
}
getfile <- function(x) {
  paste("figures/test-793725c06f", sprintf("%03d", x), ".pdf", sep="")
}
theme_sample <- function() {
  theme_bw() + theme(legend.title=element_blank(),
                     panel.border=element_blank(),
                     panel.grid.major.y=element_line(color="gray80"),
                     panel.grid.major.x=element_line(color="gray80"),
                     panel.grid.minor=element_blank(),
                     axis.ticks.length = unit(0, "cm"),
                     axis.ticks.margin = unit(0.1, "cm"),
                     legend.background = element_rect(colour = '#FFFFFF00', fill = '#FFFFFF', size = 0.4))
}


## figure 6.1 and 6.2
if (F) {
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
  ggsave(getfile(1), plot=first, width=15, height=2)
  second <- ggplot(gdota, aes(x, y, label=label)) + geom_text(size=8, color=color2) + ylim(0.5, 4.5) + xlab("") + ylab("") + theme_plain2()
  ggsave(getfile(2), plot=second, width=15, height=2)
}

## figure 6.3
if (F) {
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
  pdf(getfile(3), width=9, height=6)
  grid.arrange(aa,bb,ee,cc,dd,ff, ncol=3, clip=T)
  dev.off()
}

## figure 6.4
if (F) {
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
  cc <- ggplot(dfi, aes(x,y)) + geom_point(size=sz, shape=shp, color="gray40", fill=as.character(cls), show_guide=F) + 
    ggtitle("Shape and Color (c)") + theme_plain()
  #pdf(getfile(4), width=9, height=3)
  grid.arrange(bb,aa,cc, ncol=3, clip=T)
  #dev.off()
}

# Figure 6.5 is not R generated

# Figure 6.6
if (F) {
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
  
  pdf(getfile(6), width=10, height=4)
  grid.arrange(aa, bb, ncol=2, clip=T)
  dev.off()  
}

