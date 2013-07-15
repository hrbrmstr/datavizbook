# 
# ch3.R
#
# sample analysis script for AlienVault IP Reputation Database data
#

# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes
setwd("~/Dropbox/datavizbook/bob-chapters/chapter3/reputation")

avURL <- "http://reputation.alienvault.com/reputation.data"

# relative path for the downloaded data
avRep <- "data/reputation.data"

# using an if{}-wrapped test with download.file() vs read.xxx() avoids
# having to re-download a 16MB file every time we run the script

if (file.access(avRep)) {
  download.file(avURL,avRep) 
}

# read in the IP reputation db into a data frame
av <- read.csv(avRep,sep="#",stringsAsFactors=FALSE,header=FALSE)


str(av)

# assign more readable column names to make it easier to work with the data 
# IP | reliability | risk | type | country | locale | coords | x
colnames(av) <- c("IP","Reliability","Risk","Type","Country","Locale","Coords","x")

# take a quick look at the data
head(av)

summary(av)

colmode <- function(n) {
  siftedUnique <- unique(n)
  siftedUnique[which.max(tabulate(match(n, siftedUnique)))]
}

summary(av$Reliability)
summary(av$Risk)

# use the fact that we can index into an R
# table() to mimic Python’s mode() function output 
colmode(av$Reliability)
table(av$Reliability)[colmode(av$Reliability)]

colmode(av$Risk)
table(av$Risk)[colmode(av$Risk)]

summary(factor(av$Reliability))
summary(factor(av$Risk))
summary(factor(av$Type))
summary(factor(av$Country))

palette = c("#BF6363", "#5DD8A3", "#589DB9", "#B4D05A", "#BD8C3B", "#B586B6", "#527E49")
barcol = palette[1]

opar = par()
par(mfrow=c(3,1))
barplot(head(summary(factor(av$Country)),20),col=palette[1],main="Summary By Country",xlab="Country")
barplot(summary(factor(av$Risk)),col=palette[2],main="Summary by 'Risk'",xlab="Node 'Risk' Level")
barplot(summary(factor(av$Reliability)),col=palette[3],main="Summary by Rating Reliability",xlab="Reliability")
par(opar)

opar <- par()
par(mar=c(5.1,15,4.1,2.1))
for(rsk in 1:7) {
  f <- summary(factor(av[(av$Risk == rsk),]$Type))
  bp <- barplot(f, horiz=TRUE, yaxt='n', col=palette[4], main=sprintf("Types by Risk Level of %d",rsk)) 
  axis(2, at=bp, labels=names(f), tick=FALSE, las=2) 
}
par(opar)

# "low" reliability and "low" (and above) risk
nrow(av[(av$Reliability>2) & (av$Risk>3),])

# "medium" reliability and "medim" (and above) risk
nrow(av[(av$Reliability>4) & (av$Risk>3),]) 

# what makes these nodes "risky"?
risky <- av[(av$Reliability>4) & (av$Risk>3),]
risky.types <- summary(factor(risky$Type))
risky.types <- risky.types[order(-risky.types)]
risky.types

opar <- par()
par(mar=c(5.1,15,4.1,2.1))
f <- summary(factor(risky$Type))
f <- f[order(-f)]
bp <- barplot(f, horiz=TRUE, yaxt='n',col=palette[5 ],main="Risky Types")
axis(2, at=bp, labels=names(f[]), tick=FALSE, las=2) 
par(opar)

risky <- av[(av$Reliability>4) & (av$Risk>3),]
# strsplit will split the Type column field into multiple
# values whenever it comes across a ";" and will return a list
# of the same length as the original column, but each entry 
# will be a vector of "splits". 
tmp <- strsplit(risky$Type,";")

# this will run the "length()" function on every element
# of our new vector and return it as a list
times <- sapply(tmp,length)

# we "re-make" the data frame by using cbind() to
# combine our individual columns, but with the added
# step of using rep() to expand each column but the
# size number of split elements on the Type field.
risky <- data.frame(cbind(IP=rep(risky$IP, times),
                          Reliability=rep(risky$Reliability, times),
                          Risk=rep(risky$Risk, times),
                          Type=unlist(tmp),
                          Country=rep(risky$Country, times),
                          Locale=rep(risky$Locale, times),
                          Coords=rep(risky$Coords, times)))
nrow(risky)
risky.types <- summary(factor(risky$Type))
risky.types <- risky.types[order(-risky.types)]
risky.types

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
par(mar=c(5.1,10,4.1,2.1),mfrow=c(2,1))

av.x.types <- summary(factor(av.expanded$Type))
av.x.types <- av.x.types[order(av.x.types)]
bp <- barplot(av.x.types, horiz=TRUE, yaxt='n',col=palette[1], main="All Nodes Broken Down By Type")
axis(2, at=bp, labels=names(av.x.types), tick=FALSE, las=2) 

risky.types <- summary(factor(risky$Type))
risky.types <- risky.types[order(risky.types)]
risky.types
bp2 <- barplot(risky.types, horiz=TRUE, yaxt='n',col=palette[1], main="Higher Risk Nodes Broken Down By Type")
axis(2, at=bp2, labels=names(risky.types), tick=FALSE, las=2) 

par(opar)


summary(factor(risky$Country))
risky.cc <- summary(factor(risky$Country))
risky.cc <- risky.cc[order(-risky.cc)]
risky.cc


us.locales <- summary(factor(risky[(risky$Country == "US"),]$Locale))
head(us.locales[order(-us.locales)],10)

# retrieve IANA prefix list
ianaURL <- "http://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv"
ianaData <- "data/ipv4-address-space.csv"
if (file.access(ianaData)) {
  download.file(ianaURL,ianaData) 
}

iana <- read.csv(ianaData,stringsAsFactors=FALSE)

# examine it
str(iana)

# clean up the iana prefix
iana$Prefix <- sub("^(00|0)","",iana$Prefix,perl=TRUE)
iana$Prefix <- sub("/8$","",iana$Prefix,perl=TRUE)
head(iana$Prefix)

# extract just the prefix from the AlienVault list
av.IP.prefix <- sapply(strsplit(av$IP,'.',fixed=TRUE),"[",1)
av$Designation <- sapply(av.IP.prefix,function(ip) {
  iana[iana$Prefix == ip,]$Designation
})

desig <- summary(factor(av$Designation))
desig <- desig[order(-desig)]
desig

df <- data.frame(table(iana$Designation),stringsAsFactors=FALSE)
colnames(df) <- c("reg","ct")
av.reg <- df[df$reg %in% names(desig),]
av.reg[with(av.reg, order(-ct)),]
