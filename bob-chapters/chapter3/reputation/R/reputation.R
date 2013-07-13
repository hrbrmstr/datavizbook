#
# reputation.R
#
# sample analysis script for AlienVault IP Reputation Database data
#

# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes
setwd("~/Dropbox/datavizbook/bob-chapters/chapter3/reputation")

library(plyr)

avURL <- "http://reputation.alienvault.com/reputation.data"

# relative path for the downloaded data
avRep <- "data/reputation.data"

# using an if{}-wrapped test with download.file() vs read.xxx() avoids
# having to re-download a 16MB file every time we run the script

if (file.access(avRep)) {
  download.file(avURL,avRep) 
}

# read in the IP reputation db into a data frame
av <- read.csv(avRep,sep="#",stringsAsFactors=FALSE)

str(av)

# assign more readable column names to make it easier to work with the data 
# IP | reliability | risk | type | country | locale | coords | x
colnames(av) <- c("IP","Reliability","Risk","Type","Country","Locale","Coords","x")

# take a quick look at the data
head(av)

summary(av)

colMode <- function(n) {
  siftedUnique <- unique(n)
  siftedUnique[which.max(tabulate(match(n, siftedUnique)))]
}

summary(av$Reliability)
summary(av$Risk)

colMode(av$Reliability)
colMode(av$Risk)

summary(factor(av$Reliability))
summary(factor(av$Risk))
summary(factor(av$Type))
summary(factor(av$Country))

# #8C510A #D8B365 #F6E8C3 #C7EAE5 #5AB4AC #01665E

# #762A83

barcol = "#8C510AFF"
barcol = "#762A83AA"

opar = par()
par(mfrow=c(3,1))
barplot(head(summary(factor(av$Country)),20),col=barcol,main="Summary By Country",xlab="Country")
barplot(summary(factor(av$Risk)),col=barcol,main="Summary by 'Risk'",xlab="Host 'Risk' Level")
barplot(summary(factor(av$Reliability)),col=barcol,main="Summary by Rating Reliability",xlab="Reliability")
par(opar)

apt <- av[grep("APT",av$Type),]
scanning <- av[grep("Scanning Host",av$Type),]
spamming <- av[grep("Spamming",av$Type),]
candc <- av[grep("C&C",av$Type),]
malware.domain <- av[grep("Malware Domain",av$Type),]
malware.ip <- av[grep("Malware IP",av$Type),]
malware.distribution <- av[grep("Malware distribution",av$Type),]
malicious.host <- av[grep("Malicious Host",av$Type),]


opar = par()
par(mar=c(5.1,15,4.1,2.1))
for(rsk in 1:7) {
  f <- summary(factor(av[(av$Risk == rsk),]$Type))
  bp <- barplot(f, horiz=TRUE, yaxt='n',main=sprintf("Types by Risk Level of %d",rsk)) 
  axis(2, at=bp, labels=names(f), tick=FALSE, las=2) 
}
par(opar)


library(maps)
df = data.frame(table(factor(av$Country)))
colnames(df) = c("country","value")
df$col = heat.colors(100)[floor(rescale(df$value,c(1,100)))]
#df$col = floor(rescale(df$value,c(1,50)))
df = df[df$country == "US",]
map("world",
    regions = df$country,
    lty = 1, lwd =1,
    boundary=TRUE,
    fill=TRUE,
    col=df$col)

