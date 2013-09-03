library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gdata)
library(RColorBrewer)
library(portfolio) # for treemap only

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

# figure 6.7 was not generated in R

# figure 6.8
if (F) {
  # Note: this does not use ggplot, but instead
  # creates a blank canvas and puts text and boxes on it.
  # a neat trick for truly unique approaches.
  pdf(getfile(8), width=8, height=2)
  par(mar=c(0,0,0,0))
  plot(NULL, xlim=c(0,120), ylim=c(50,100), yaxt="n", ann=FALSE, xaxt="n", bty="n")
  text(17.5,89, "Sequantial", pos=3)
  blue <- brewer.pal(5, "Blues")
  rdpu <- brewer.pal(5, "RdPu")
  ygb <- brewer.pal(5, "YlGnBu")
  text(30,83, "Blues", pos=4)
  text(30,73, "RdPu", pos=4)
  text(30,63, "YlGnBu", pos=4)
  for (i in seq(5)) {
    rect(i*5, 80, (i*5)+5, 86, col=blue[i], border=NA)
    rect(i*5, 70, (i*5)+5, 76, col=rdpu[i], border=NA)
    rect(i*5, 60, (i*5)+5, 66, col=ygb[i], border=NA)
  }
  text(57.5,89, "Diverging", pos=3)
  blue <- brewer.pal(5, "RdBu")
  rdpu <- brewer.pal(5, "PiYG")
  ygb <- brewer.pal(5, "BrBG")
  text(70,83, "RdBu", pos=4)
  text(70,73, "PiYG", pos=4)
  text(70,63, "BrBG", pos=4)
  for (i in seq(5)) {
    rect(40+(i*5), 80, (i*5)+45, 86, col=blue[i], border=NA)
    rect(40+(i*5), 70, (i*5)+45, 76, col=rdpu[i], border=NA)
    rect(40+(i*5), 60, (i*5)+45, 66, col=ygb[i], border=NA)
  }
  text(97.5,89, "Qualitative", pos=3)
  blue <- brewer.pal(5, "Set1")
  rdpu <- brewer.pal(5, "Set2")
  ygb <- brewer.pal(5, "Accent")
  text(110,83, "Set1", pos=4)
  text(110,73, "Set2", pos=4)
  text(110,63, "Accent", pos=4)
  for (i in seq(5)) {
    rect(80+(i*5), 80, (i*5)+85, 86, col=blue[i], border=NA)
    rect(80+(i*5), 70, (i*5)+85, 76, col=rdpu[i], border=NA)
    rect(80+(i*5), 60, (i*5)+85, 66, col=ygb[i], border=NA)
  }
  par(mar=c(5.1,4.1,4.1,2.1))
  dev.off()
}

# figure 6.9
if (F) {
  fw <- read.csv("data/fivemin.csv", header=T)
  fullfw <- aggregate(cbind(packets, bytes) ~ hour, data=fw, FUN=sum)
  gg <- ggplot(fullfw, aes(packets, bytes))
  gg <- gg + geom_point(size=3, color="#000066")
  gg <- gg + xlab("Packets") + ylab("Bytes") 
  gg <- gg + scale_x_continuous(breaks=c(12,15,18,21,24,27)*10^6, labels=scale.filter.nb)
  gg <- gg + scale_y_continuous(breaks=c(7,10,13,16,19)*10^9, labels=scale.filter)
  gg <- gg + theme_sample()
  
  #print(gg)
  ggsave(getfile(figure), gg, width=8, height=5)
}

# figure 6.10
if(F) {
  fw <- read.csv("data/fivemin.csv", header=T)
  fullfw <- aggregate(cbind(packets, bytes, sessions) ~ hour, data=fw, FUN=sum)
  fullfw <- fullfw[order(fullfw$hour), ] # want to be sure we're still sorted
  fullfw$iter <- seq_along(fullfw$hour)
  
  # Not a rolling average
  gcolor=rep("gray97", nrow(fullfw))
  gcolor[seq(1,nrow(fullfw), by=12)] <- "gray80"
  pcolor=rep("gray60", nrow(fullfw))
  pcolor[seq(1,nrow(fullfw), by=6)] <- "#CC0000"
  
  mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
  
  gg <- ggplot(fullfw, aes(iter, sessions))
  gg <- gg + geom_bar(stat="identity", width=0.1, color=gcolor)
  gg <- gg + geom_point(size=2, color=pcolor)
  gg <- gg + ylab("Sessions") + xlab("Time")
  gg <- gg + scale_x_continuous(labels=mylabel, 
                                breaks=seq(1,nrow(fullfw), by=12))
  gg <- gg + scale_y_continuous(labels=scale.filter.nb)
  gg <- gg + theme_bw() + theme_sample()
  print(gg)
  
  ggsave(getfile(10), gg, width=8, height=2.5)  
}

# figure 6.11
if (F) {
  fw <- read.csv("data/fivemin.csv", header=T)
  better <- data.frame(hour=unique(fw$hour), realx=seq_along(unique(fw$hour)))
  allfw <- merge(fw, better, allx=T)
  mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
  myb <- seq(1, length(unique(allfw$realx)), by=12)
  
  gg <- ggplot(allfw, aes(realx, bytes, group=type, color=type))
  gg <- gg + geom_smooth(stat="identity", fill="white")
  gg <- gg + theme_bw() + scale_y_log10(labels=scale.filter)
  gg <- gg + scale_x_continuous(breaks=myb, label=mylabel)
  gg <- gg + xlab("Time") + ylab("Bytes")
  gg <- gg + theme_sample() + theme(legend.position = "bottom")
  print(gg)
  # ggsave(getfile(11), gg, width=8, height=5)
  aa <- gg
  
  gg <- ggplot(allfw, aes(realx, bytes, group=type, color=type))
  gg <- gg + geom_point(size=1.5) #(stat="identity", fill="white")
  gg <- gg + theme_bw() + scale_y_log10(labels=scale.filter)
  gg <- gg + scale_x_continuous(breaks=myb, label=mylabel)
  gg <- gg + xlab("Time") + ylab("Bytes")
  gg <- gg + theme_sample() + theme(legend.position = "bottom")
  print(gg)
  pdf(getfile(11), width=11, height=5)
  grid.arrange(gg, aa, ncol=2, clip=T)
  dev.off()
}

# Figure 6.12

if (F) {
  wk <- c(56, 61, 44, 50, 37, 48,  2,  2,  1,  2,  2,  1)
  dev <- c("Workstation", "Server", "Network", "Printer")
  sev <- c("High", "Med", "Low")
  dfi <- data.frame(x=rep(dev, each=3), y=wk, sev=rep(rev(sev), 4))
  dfi$x <- factor(dfi$x, levels=dev, ordered=T)
  dfi$sev <- factor(dfi$sev, levels=sev, ordered=T)
  color.pal <- brewer.pal(3, "Reds")
  
  gg <- ggplot(dfi, aes(x, y))
  gg <- gg + geom_bar(stat="identity", fill=color.pal[2], show_guide=F)
  gg <- gg + xlab("Device Type") + ylab("Vulnerabilities")
  gg <- gg + theme_sample() + theme(plot.margin = unit(c(0.5,0.4,2,0), "cm"))
  gg <- gg + ggtitle("Vertical Bar Chart")
  # print(gg)
  aa <- gg
  gg <- ggplot(dfi, aes(x, y, fill=sev))
  gg <- gg + geom_bar(stat="identity") #  show_guide=F)
  gg <- gg + xlab("Device Type") + ylab("Vulnerabilities")
  gg <- gg + scale_fill_manual(values=rev(color.pal))
  gg <- gg + theme_sample() + theme(legend.position = "bottom")
  gg <- gg + ggtitle("Stacked Bar Chart")
  #print(gg)
  bb <- gg
  gg <- ggplot(dfi, aes(x, y, fill=sev))
  gg <- gg + geom_bar(stat="identity", position=position_dodge()) #  show_guide=F)
  gg <- gg + xlab("Device Type") + ylab("Vulnerabilities")
  gg <- gg + scale_fill_manual(values=rev(color.pal))
  gg <- gg + theme_sample() + theme(legend.position = "bottom")
  gg <- gg + ggtitle("Grouped Bar Chart")
  #print(gg)
  cc <- gg
  
  pdf(getfile(12), width=12, height=4)
  # making this bigger, when smaller the font overlaps
  grid.arrange(aa, bb ,cc, ncol=3, clip=T)
  dev.off()  
}

# figure 6.13, bubbles
if (F) {
  fw <- read.csv("data/fivemin.csv", header=T)
  wk <- fw[which(fw$type=="Network"), ]  # not log
  myred <- brewer.pal(3, "Set2")[3]
  
  gg <- ggplot(wk, aes(sessions, bytes, size=packets, color=type, fill=type))
  gg <- gg + xlab("Sessions") + ylab("Bytes")
  gg <- gg + geom_point(alpha=1/3, shape=21, fill=myred, color="gray80", guide=F)
  gg <- gg + theme_bw()
  gg <- gg + scale_size_continuous(name="Packet Count", range = c(1, 20), trans=log10_trans())
  gg <- gg + ggtitle("alpha = 1/3")
  gg <- gg + scale_x_continuous(labels=scale.filter.nb)
  gg <- gg + scale_y_continuous(labels=scale.filter, limits=c(0, max(wk$bytes)*1.1)) 
  gg <- gg + theme_sample() + theme(legend.position="bottom", legend.title = element_text(colour="black", size=10))
  bb <- gg
  
  gg <- ggplot(wk, aes(sessions, bytes, size=packets, color=type, fill=type))
  gg <- gg + xlab("Sessions") + ylab("Bytes")
  gg <- gg + geom_point(alpha=1, shape=21, fill=myred, color="gray80", guide=F)
  gg <- gg + theme_bw()
  gg <- gg + scale_size_continuous(name="Packet Count", breaks=c(1,10), range = c(1, 20), trans=log10_trans())
  gg <- gg + ggtitle("alpha = 1")
  gg <- gg + scale_x_continuous(labels=scale.filter.nb)
  gg <- gg + scale_y_continuous(labels=scale.filter, limits=c(0, max(wk$bytes)*1.1)) 
  gg <- gg + theme_sample() + theme(legend.position="bottom", legend.title = element_text(colour="black", size=10))
  aa <- gg
  #print(gg)
  pdf(getfile(13), width=9, height=5)
  grid.arrange(aa, bb, ncol=2, clip=T)
  dev.off()
}

# Figure 6.14
if (F) {
  mydata <- read.csv("data/ipmap2.csv", header=T)
  mydata$mean <- mydata$sessions/mydata$count
  # this adds stuff, I removed it with inkscape before sending to publisher
  pdf(getfile(14), width=9, height=5)
  map.market(id=mydata$label, area=sqrt(mydata$count)^1.3, group=mydata$type, 
             color=sqrt(mydata$mean), main="remove all but treemap", 
             lab= c("group"=F, "id"=T))
  dev.off()
}

# figure 6.15
if (F) {
  fw <- read.csv("data/fivemin.csv", header=T)
  fullfw <- aggregate(cbind(packets, bytes, sessions) ~ hour, data=fw, FUN=sum)
  aa <- ggplot(fullfw, aes(x=sessions))
  #aa <- aa + geom_density() + theme_sample()
  aa <- aa + geom_histogram(binwidth=12000, colour="black", fill="#8DA0CB") 
  aa <- aa + scale_x_continuous(labels=scale.filter.nb)
  aa <- aa + xlab("Number of Sessions") + ylab("Count") + theme_sample()
  #print(aa)
  
  gg <- ggplot(fullfw, aes(x=sessions))
  gg <- gg + geom_histogram(aes(y=..density..), binwidth=12000, colour="#80808080", fill="#8DA0CB66", alpha=1/3)
  gg <- gg + geom_density(alpha=1/2, fill="#8DA0CB")  # Overlay with transparent density plot
  gg <- gg + xlab("Number of Sessions") + ylab("Density")
  gg <- gg + scale_x_continuous(labels=scale.filter.nb)
  gg <- gg + theme_sample()
  #print(gg)
  
  pdf(getfile(15), width=8, height=4)
  grid.arrange(aa, gg, ncol=2, clip=T)
  dev.off()
}

# figure 6.16 and 6.17 were created in R, but not for the book

# figure 6.18
if (F) {
  my.fw <- read.csv("data/3weeks.csv", header=T)
  my.fw2 <- aggregate(bytes ~ dh.seq, data=my.fw, FUN=mean)
  aa.breaks <- 144+seq(1,nrow(my.fw), by=nrow(my.fw)/21)
  aa.lab <- rep(c("S", "M", "T", "W", "T", "F", "S"), 3)
  bb.breaks <- nrow(my.fw2)/42 + seq(1, nrow(my.fw2), by=nrow(my.fw2)/21)
  
  aa <- ggplot(my.fw, aes(x=seq, y=bytes)) + geom_line(color="steelblue") + theme_bw() + 
    scale_y_continuous(labels=scale.filter, limits=c(0, max(my.fw$bytes))) + ggtitle("Basic Line Plot") +
    scale_x_continuous(labels=aa.lab, breaks=aa.breaks) + xlab("Day") + ylab("Bytes")
  print(aa)
  
  bb <- ggplot(my.fw2, aes(x=dh.seq, y=bytes)) + geom_line(color="steelblue") + theme_bw() + 
    scale_y_continuous(labels=scale.filter, limits=c(0, max(my.fw$bytes))) + ggtitle("One hour averages") +
    scale_x_continuous(labels=aa.lab, breaks=bb.breaks) + xlab("Day") + ylab("Bytes")
  
  cc <- ggplot(my.fw, aes(x=seq, y=bytes)) + geom_point(alpha=2/3, size=1, color="steelblue") + 
    theme_bw() + ggtitle("Using Points") + 
    scale_y_continuous(labels=scale.filter, limits=c(0, max(my.fw$bytes))) +
    scale_x_continuous(labels=aa.lab, breaks=aa.breaks) + xlab("Day") + ylab("Bytes")
  
  pdf(getfile(18), width=9, height=7)
  grid.arrange(aa, bb, cc, ncol=1, clip=T)
  dev.off()
}

# Movie stuff (pasted from book)
if (F) {
  # random walk
  set.seed(1)
  # set up nine directions
  dirs <- matrix(c(rep(seq(-1, 1), 3), 
                   rep(seq(-1, 1), each=3)), ncol=2, byrow=T)
  # start in the center
  cpos <- matrix(c(0, 0), ncol=2)
  # set full screen
  par(mar=c(0,0,0,0))
  for(i in seq(200)) { 
    plot(cpos, type="p", col="gray80", xlim=c(-20, 20), ylim=c(-20,20),
         yaxt="n", ann=FALSE, xaxt="n", bty="n")
    cpos <- rbind(cpos, cpos[nrow(cpos), ] + dirs[sample(1:9, 1), ])
    points(cpos[nrow(cpos), 1], cpos[nrow(cpos), 2],
           type="p", pch=16, col="red")
    Sys.sleep(0.1)
  }
  # reset screen back to default
  par(mar=c(5.1,4.1,4.1,2.1))
}

