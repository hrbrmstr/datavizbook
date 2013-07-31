options(width=70)
setwd("~/Dropbox/datavizbook/chapters")
# retrieve IANA prefix list
ianaURL <- "http://www.iana.org/assignments/\
ipv4-address-space/ipv4-address-space.csv"
ianaData <- "ch04/data/ipv4-address-space.csv"
if (file.access(ianaData)) {
  download.file(ianaURL,ianaData) 
}

# read in the IANA table
iana <- read.csv(ianaData)

# clean up the iana prefix
iana$Prefix <- sub("^(00|0)","",iana$Prefix,perl=TRUE)
iana$Prefix <- sub("/8$","",iana$Prefix,perl=TRUE)

# re-read the existing AlienVault data
avRep <- "ch03/data/reputation.data"
av <- read.csv(avRep,sep="#",header=FALSE)
colnames(av) <- c("IP","Reliability","Risk","Type",
                  "Country","Locale","Coords","x")

# extract just the prefix from the AlienVault list
av.IP.prefix <- sapply(strsplit(as.character(av$IP),'.',
                                fixed=TRUE),"[",1)
av$Designation <- sapply(av.IP.prefix,function(ip) {
  iana[iana$Prefix == ip,]$Designation
})

desig <- summary(factor(av$Designation))
desig <- desig[order(-desig)]
desig

iana.df <- data.frame(table(iana$Designation),stringsAsFactors=FALSE)
colnames(iana.df) <- c("reg","ct")
av.reg <- iana.df[iana.df$reg %in% names(desig),]
av.reg[with(av.reg, order(-ct)),]
