census <- read.csv("~/mac/book/data/src/ip-to-census.csv", header=T)

census$high <- census$high*0.01*census$pop
census$college <- census$college*0.01*census$pop

ggplot(data=census, aes(x=count)) + geom_histogram(color="white") + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()

ggplot(data=census, aes(x=college)) + geom_histogram(color="white") + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()

# 

# bob
av <- read.csv("~/Documents/github/datavizbook/bob-chapters/chapter3/ch3/data/reputation.data",sep="#")

colnames(av) <- c("IP","Reliability","Risk", "Type","Country","Locale","Coords","x")

library(aplpack)
stemleaf <- function(x, width=70) {
  xtbl <- data.frame(table(x)) 
  xpad <- max(nchar(as.character(xtbl$x)))
  Freqpad <- max(nchar(xtbl$Freq))
  padding <- width - (2 + xpad + Freqpad)
  xtbl$tspace = round((padding/max(xtbl$Freq))*xtbl$Freq)
  buf <- paste("%", xpad, "s %-", padding, "s %-s", sep="")
  apply(xtbl, 1, function(y) sprintf(buf, y['x'], 
                                     paste(rep("*", y['tspace']), collapse=""), 
                                     y['Freq']))
}


headSummary <- function(x, length=6) {
  x.factor <- summary(factor(x))  # count and organize the data
  x.sorted <- sort(x.factor, decreasing=T)  # sort the table in descending order
  head(x.sorted, length)  # show the first length values
}
Country.table <- table(av$Country)

mycolors <- rev(c("yellow1","yellow2","yellow3","yellow4", 
              "thistle1","thistle2","thistle3"))
plot(table(av$Reliability, av$Risk), col=mycolors, main="Risk and Reliability")

# hmmm
# http://stats.stackexchange.com/questions/4211/how-to-visualize-3d-contingency-matrix/20900#20900
heatmap(table(av$Reliability, av$Risk))

foo <- as.data.frame(table(av$Reliability, av$Risk))
colnames(foo) <- c("Reliability", "Risk", "Freq")
ggplot(foo, aes(Reliability, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=Freq))

foo <- as.data.frame(table(av$Reliability, av$Risk))
colnames(foo) <- c("Reliability", "Risk", "Freq")
foosum <- sum(foo$Freq)
foo$Risk <- as.numeric(foo$Risk)
foo$Reliability <- as.numeric(foo$Reliability)
foo$prob <- apply(foo, 1, function(x) round(sum(foo$Freq[which(foo$Risk>x['Risk'] & foo$Reliability>x['Reliability'])])/foosum, 4)*100)
ggplot(foo, aes(Reliability, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=prob))
foo$tot <- apply(foo, 1, function(x) sum(foo$Freq[which(foo$Risk>x['Risk'] & foo$Reliability>x['Reliability'])]))
ggplot(foo, aes(Reliability, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=tot))

av$newtype <- av$Type
av$newtype[grep(';', av$newtype)] <- "Multiples"
foo <- as.data.frame(table(av$newtype, av$Risk))
colnames(foo) <- c("Type", "Risk", "Freq")
ggplot(foo, aes(Type, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=Freq))

goo <- foo[which(foo$Type!="Scanning Host"), ]
ggplot(goo, aes(Type, Freq)) + geom_bar() + facet_grid(Type ~ .)

foo$logFreq <- log2(foo$Freq)
foo$logFreq <- ifelse(is.infinite(foo$logFreq), 0, round(foo$logFreq, 1))
ggplot(foo, aes(Risk, Reliability)) + geom_tile(aes(fill=logFreq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=Freq))



hoo <-table(av$Reliability, av$Risk)
goo <- as.matrix(table(av$Reliability, av$Risk))
goo <- as.matrix(table(av$Risk, av$Reliability))
heatmap(goo, Rowv=NA, Colv=NA, col = rev(topo.colors(256)), scale="column")
heatmap(goo, Rowv=NA, Colv=NA, col=rainbow(124)[1:24])
#, margins=c(5,10))

mytable <- xtabs(~Risk+Reliability, data=av)
ftable(mytable) # print table 
summary(mytable) # chi-square test of indepedence

library(vcd)
mosaic(table(av$Risk, av$Reliability), shade=TRUE, legend=TRUE)
assoc(table(av$Risk, av$Reliability), shade=TRUE)

tmp <- strsplit(av$Type,";")
times <- sapply(tmp,length)
av.expanded <- data.frame(
  cbind(IP=rep(av$IP, times),
        Reliability=rep(av$Reliability, times),
        Risk=rep(av$Risk, times),
        Type=unlist(tmp),
        Country=rep(av$Country, times),
        Locale=rep(av$Locale, times),
        Coords=rep(av$Coords, times),stringsAsFactors=FALSE),
  stringsAsFactors=FALSE)

opar <- par()

# adjust plotting margins and setup 2x1 grid
par(mar=c(5.1,10,4.1,2.1),mfrow=c(2,1))

av.x.types <- summary(factor(av.expanded$Type))
av.x.types <- av.x.types[order(av.x.types)]

# draw the boxplot horizontally and adjust labels
bp <- barplot(av.x.types, horiz=TRUE, yaxt='n',
              col=barcol, main="All Nodes Broken Down By Type")
axis(2, at=bp, labels=names(av.x.types), tick=FALSE, las=2)

risky.types <- summary(factor(risky$Type))
risky.types <- risky.types[order(risky.types)]
risky.types
bp2 <- barplot(risky.types, horiz=TRUE, yaxt='n',
               col=barcol, main="Higher Risk Nodes Broken Down By Type")
axis(2, at=bp2, labels=names(risky.types), tick=FALSE, las=2)

par(opar)






