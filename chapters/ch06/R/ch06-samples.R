runall <- FALSE
figure <- 7  # start at figure number...

scale.filter <- function(x) {
  x[is.na(x)] <- 0
  humanReadable(x)
}
scale.filter.nb <- function(x) {
  x[is.na(x)] <- 0
  sub("B", "", humanReadable(x))
}
getfile <- function(x) {
  paste("figures/793725c06f", sprintf("%03d", x), ".pdf", sep="")
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

# load source data
fw <- read.csv("data/fivemin.csv", header=T)

if (runall) {
  fullfw <- aggregate(cbind(packets, bytes, sessions) ~ hour, data=fw, FUN=sum)
  fullfw <- fullfw[order(fullfw$hour), ] # want to be sure we're still sorted
  fullfw$iter <- seq_along(fullfw$hour)
  
  # rolling average (2)
  x <- seq(2, nrow(fullfw))
  y <- sapply(x, function(z) sum(fullfw$sessions[(z-1):z])/2)
  dfi <- data.frame(x, y)
  gcolor=rep("gray95", nrow(dfi))
  gcolor[seq(1,nrow(dfi), by=12)] <- "gray80"
  pcolor=rep("gray60", nrow(dfi))
  pcolor[seq(1,nrow(dfi), by=6)] <- "#CC0000"
  mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
  
  gg <- ggplot(dfi, aes(x, y))
  gg <- gg + geom_bar(stat="identity", width=0.1, color=gcolor)
  gg <- gg + geom_point(size=2, color=pcolor)
  gg <- gg + ylab("Sessions") + xlab("Time")
  gg <- gg + scale_x_continuous(labels=mylabel, breaks=seq(2,nrow(dfi), by=12))
  gg <- gg + scale_y_continuous(labels=scale.filter.nb)
  gg <- gg + theme_sample()
  print(gg)
  ggsave(getfile(figure), gg, width=8, height=3)
  # Not a rolling average
  #   gcolor=rep("gray97", nrow(fullfw))
  #   gcolor[seq(1,nrow(bob), by=12)] <- "gray80"
  #   pcolor=rep("gray60", nrow(fullfw))
  #   pcolor[seq(1,nrow(bob), by=6)] <- "#CC0000"
  #   
  #   ggplot(fullfw, aes(iter, sessions)) + geom_bar(stat="identity", width=0.1, color=gcolor) + 
  #     geom_point(size=3, color=pcolor) + theme_bw() + ylab("Sessions") + xlab("") +
  #     scale_x_continuous(labels=mylabel, breaks=seq(1,nrow(dfi), by=12)) +
  #     scale_y_continuous(labels=c("0", "100k", "200k", "300k", "400k"))
}
figure <- figure + 1

  

if (runall) {
  fullfw <- aggregate(cbind(packets, bytes) ~ hour, data=fw, FUN=sum)
  gg <- ggplot(fullfw, aes(packets, bytes))
  gg <- gg + geom_point(size=3, color="#000066")
  gg <- gg + xlab("Packets") + ylab("Bytes") 
  gg <- gg + scale_x_continuous(breaks=c(12,15,18,21,24,27)*10^6, labels=scale.filter.nb)
  gg <- gg + scale_y_continuous(breaks=c(7,10,13,16,19)*10^9, labels=scale.filter)
  gg <- gg + theme_sample()
  
  print(gg)
  ggsave(getfile(figure), gg, width=8, height=5)
}
figure <- figure + 1

if (runall) {
  ## line
  gcolor[seq(1,nrow(bob), by=12)] <- "gray60"
  pcolor=rep("gray60", nrow(bob))
  pcolor[seq(1,nrow(bob), by=6)] <- "#CC0000"
  mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
  myb <- seq(1, length(unique(allfw$realx)), by=12)
  better <- data.frame(hour=unique(fw$hour), realx=seq_along(unique(fw$hour)))
  allfw <- merge(fw, better)
  
  gg <- ggplot(fwspec, aes(realx, bytes, group=type, color=type))
  gg <- gg + geom_smooth(stat="identity", fill="white") + theme_bw() + scale_y_log10(labels=scale.filter)
  gg <- gg + scale_x_continuous(breaks=myb, label=mylabel)
  gg <- gg + xlab("Time") + ylab("Bytes")
  gg <- gg + theme_sample()
  print(gg)
  ggsave(getfile(figure), gg, width=8, height=5)
}
figure <- figure + 1

# bar chart
if (runall) {
  # maybe pull a different color blue here?
  fw$realhour <- substr(fw$hour, 1, 2)
  barfw <- aggregate(cbind(packets, bytes, sessions) ~ type + realhour, data=fw, FUN=sum)
  barfw$type <- factor(barfw$type, levels=c("Workstation", "Server", "Network", "Printer"), ordered=T)
  allbarfw <- aggregate(cbind(packets,bytes,sessions) ~ realhour, data=barfw, FUN=sum)
  gg <- ggplot(allbarfw, aes(realhour, sessions))
  gg <- gg + geom_bar(stat="identity", fill="#000066", show_guide=F)
  gg <- gg + xlab("Hour") + ylab("Sessions")
  gg <- gg + scale_y_continuous(labels=scale.filter.nb)
  gg <- gg + scale_x_discrete(labels=mylabel)
  gg <- gg + theme_sample()
  print(gg)
  aa <- gg
  gg <- ggplot(allbarfw, aes(rev(realhour), sessions))
  gg <- gg + geom_bar(stat="identity", fill="#000066", show_guide=F)
  gg <- gg + xlab("Hour") + ylab("Sessions")
  gg <- gg + scale_y_continuous(labels=scale.filter.nb)
  gg <- gg + scale_x_discrete(labels=rev(mylabel))
  gg <- gg + theme_sample() + coord_flip()
  print(gg)
  bb <- gg
  #pdf("figures/793725c06f003.pdf", width=9, height=6)
  pdf(getfile(figure), width=8, height=3)
  grid.arrange(aa, bb, ncol=2, clip=T, widths=c(7,8))
  dev.off()
  figure <- figure + 1
  
  gg <- ggplot(barfw, aes(realhour, log10(sessions), fill=type))
  gg <- gg + geom_bar(stat="identity") #, show_guide=F)
  gg <- gg + scale_fill_brewer(palette="Set2")
  gg <- gg + xlab("Time") + ylab("")
  gg <- gg + scale_y_continuous(labels=scale.filter.nb)
  gg <- gg + scale_x_discrete(labels=mylabel)
  gg <- gg + theme_sample() + theme(axis.text.y = element_blank())
  print(gg)
  cc <- gg
  
  gg <- ggplot(barfw, aes(realhour, sessions, fill=type))
  gg <- gg + geom_bar(stat="identity", position=position_dodge(), show_guide=F)
  gg <- gg + scale_fill_brewer(palette="Set2")
  gg <- gg + xlab("Time") + ylab("Sessions")
  gg <- gg + scale_y_log10(labels=scale.filter.nb)
  gg <- gg + scale_x_discrete(labels=mylabel)
  gg <- gg + theme_sample() #+ theme(legend.position="bottom")
  print(gg)
  dd <- gg
  pdf(getfile(figure), width=12, height=5)
  # making this bigger, when smaller the font overlaps
  grid.arrange(dd, cc, ncol=2, clip=T, widths=c(8,6))
  dev.off()
  figure <- figure + 1
} else {
  figure <- figure + 2
}

if (runall) {
  ##
  ## size
  ##
  wk <- fw[which(fw$type=="Network"), ]  # not log
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
  ggsave(getfile(figure), gg, width=8, height=5)
}
figure <- figure + 1

## log plot
#foo <- c(seq(1,10), seq(1,10)*10, seq(1,10)*100)
#dfi <- data.frame(y=c(foo, log10(foo)), x=rep(c("a", "b"), each=length(foo)*2), g=rep(seq(1,length(foo)*2), 2))

if (runall) {
  # histograms
  fullfw <- aggregate(cbind(packets, bytes, sessions) ~ hour, data=fw, FUN=sum)
  aa <- ggplot(fullfw, aes(x=sessions))
  #aa <- aa + geom_density() + theme_sample()
  aa <- aa + geom_histogram(binwidth=10000, colour="black", fill="#8DA0CB") 
  aa <- aa + scale_x_continuous(labels=scale.filter.nb)
  aa <- aa + xlab("Number of Sessions in 5 minutes\n(binwidth=10k)") + ylab("Count") + theme_sample()
  #print(aa)
  
  gg <- ggplot(fullfw, aes(x=sessions))
  gg <- gg + geom_histogram(aes(y=..density..), binwidth=12000, colour="gray50", fill="#8DA0CB66")
  gg <- gg + geom_density(alpha=1/10, fill="#FF6666")  # Overlay with transparent density plot
  gg <- gg + xlab("Number of Sessions in 5 minutes\n(binwidth=12k)") + ylab("Density")
  gg <- gg + scale_x_continuous(labels=scale.filter.nb)
  gg <- gg + theme_sample()
  #print(gg)
  
  pdf(getfile(figure), width=8, height=4)
  grid.arrange(aa, gg, ncol=2, clip=T)
  dev.off()
}
figure <- figure + 1

# simple box plot (replacing with previous work)
if (0) {
  gg <- ggplot(fullfw, aes(x=foo, y=sessions)) + coord_flip()
  gg <- gg + scale_y_continuous(labels=scale.filter.nb)
  gg <- gg + geom_boxplot(fill="#8DA0CB") + theme_sample() + theme(axis.text.y=element_blank())
  gg <- gg + xlab("") + ylab("Number of Sessions in 5 minutes")
  gg <- gg + theme(panel.grid.major.y=element_blank())
  print(gg)
  ggsave(getfile(figure), gg, width=8, height=2)  
}

if (0) {
  ## shape
  
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
  
}

if (0) {
#   foo <- read.csv("~/Documents/book/bobfw/forjay-network.csv", header=T)
#   goo <- data.frame(ip=foo[[7]], sys=foo[[3]], type="Network")
#   foo <- read.csv("~/Documents/book/bobfw/forjay-servers.csv", header=T)
#   goo <- rbind(goo, data.frame(ip=foo[[15]], sys=foo[[32]], type="Server"))
#   
#   write.csv(goo, "~/Documents/book/bobfw/ip-map.csv", row.names=F)
#   forjay-servers.csv
#   #[8/29/13 10:17:02 AM] Bob Rudis: Windows 7 Task Worker
#   #[8/29/13 10:17:08 AM] Bob Rudis: Windows 7 Developer Workstation
#   #[8/29/13 10:17:17 AM] Bob Rudis: Linux Developer Workstation
#   
#   foo <- read.csv("~/Documents/book/bobfw/treemap.csv", header=T)
#   foo$count <- 1
#   foo <- foo[which(foo$sessions>0), ]
#   mine <- aggregate(cbind(count, sessions) ~ cat + type, data=foo, FUN=sum)
#   mine$mean <- mine$sessions/mine$count
  #map.market(id=mine$cat, area=mine$count, group=mine$type, color=mine$mean, main="what the")

#  write.csv(mine, "~/Documents/book/bobfw/ipmap2.csv", row.names=F)
  foo <- read.csv("~/Documents/book/bobfw/ipmap2.csv", header=T)
  foo$mean <- foo$sessions/foo$count
  map.market(id=foo$label, area=sqrt(foo$count)^1.3, group=foo$type, color=sqrt(foo$mean), main="what the", lab= c("group"=F, "id"=T))
  map.market(id=foo$label, area=foo$count, group=foo$type, color=sqrt(foo$mean), main="what the", lab= c("group"=F, "id"=T))
}

if (runall) {
  one <- read.csv("~/Documents/book/bobfw/onemin2.csv", header=T)
  allone <- aggregate(bytes ~ hour, data=one, FUN=sum)
  allone <- allone[with(allone, order(hour)), ]
  allone$iter <- seq_along(allone$hour)
  mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
  mybreaks <- seq(1, nrow(allone), by=60)
  gg <- ggplot(allone, aes(iter, bytes)) 
  gg <- gg + geom_line()
  gg <- gg + ylab("Bytes") + xlab("Time")
  gg <- gg + scale_x_continuous(labels=mylabel, breaks=mybreaks)
  gg <- gg + scale_y_continuous(labels=scale.filter, limits=c(0,max(allone$bytes)))
  gg <- gg + theme_sample()
  print(gg)
  aa <- gg
  #
  # average 2
  rollav <- function(x, by=2) {
    sapply(seq(by,length(x)), function(z) mean(x[(z-by):z]))
  }
  rez <- rollav(allone$bytes, 2)
  av2 <- data.frame(iter=seq_along(rez), bytes=rez)
  mybreaks <- seq(1, nrow(av2), by=60)
  gg <- ggplot(av2, aes(iter, bytes)) 
  gg <- gg + geom_line()
  gg <- gg + ylab("Bytes") + xlab("Time")
  gg <- gg + scale_x_continuous(labels=mylabel, breaks=mybreaks)
  gg <- gg + scale_y_continuous(labels=scale.filter, limits=c(0,max(allone$bytes)))
  gg <- gg + theme_sample()
  print(gg)
  bb <- gg
  #
  # average 3
  rez <- rollav(allone$bytes, 5)
  av2 <- data.frame(iter=seq_along(rez), bytes=rez)
  mybreaks <- seq(1, nrow(av2), by=60)
  gg <- ggplot(av2, aes(iter, bytes)) 
  gg <- gg + geom_line()
  gg <- gg + ylab("Bytes") + xlab("Time")
  gg <- gg + scale_x_continuous(labels=mylabel, breaks=mybreaks)
  gg <- gg + scale_y_continuous(labels=scale.filter, limits=c(0,max(allone$bytes)))
  gg <- gg + theme_sample()
  print(gg)
  cc <- gg
  aa <- aa + ggtitle("Total Bytes per Minute")
  bb <- bb + ggtitle("Moving Average over 2 Mins")
  cc <- cc + ggtitle("Moving Average over 5 Mins")
  pdf(getfile(17), width=8, height=6)
  grid.arrange(aa, bb, cc, ncol=1, clip=T)
  dev.off()
  
}

if (runall) {
  one <- read.csv("~/Documents/book/bobfw/onemin2.csv", header=T)
  allone <- aggregate(bytes ~ hour, data=one, FUN=sum)
  allone <- allone[with(allone, order(hour)), ]
  allone$iter <- seq_along(allone$hour)
  mylabel <- c("8am", "9am", "10am", "11am", "12pm", "1pm", "2pm", "3pm")
  mybreaks <- seq(1, nrow(allone), by=60)
  gg <- ggplot(allone, aes(iter, bytes, color=bytes)) 
  gg <- gg + geom_line(alpha=1/30, size=4)
  gg <- gg + geom_line(alpha=1/20, size=2)
  gg <- gg + geom_point(alpha=1/10, size=6)
  gg <- gg + geom_point(alpha=1/2, size=2)
  gg <- gg + ylab("Bytes") + xlab("Time")
  gg <- gg + scale_x_continuous(labels=mylabel, breaks=mybreaks)
  gg <- gg + scale_y_continuous(labels=scale.filter, limits=c(0,max(allone$bytes)))
  gg <- gg + theme_sample() + theme(legend.position="none")
  print(gg)
  