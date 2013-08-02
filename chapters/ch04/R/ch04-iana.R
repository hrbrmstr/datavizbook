library(reshape)
library(ggplot2)

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

iana.df <- data.frame(table(iana$Designation))
colnames(iana.df) <- c("Registry","IANA.Block.Count")
av.reg <- iana.df[iana.df$reg %in% names(desig),]

tmp.df = data.frame(table(factor(av$Designation)))
colnames(tmp.df) <- c("Registry","AlienVault.IANA.Count")

combined.df = merge(iana.df,tmp.df)
combined.df[with(combined.df, order(-IANA.Block.Count)),]

melted.df = melt(combined.df)

ggplot(data=melted.df) +
  geom_bar(aes(x=Registry, y=value, fill=variable), stat="identity") +
  facet_wrap(~variable, scales="free_y") +
  labs(y="Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none")

cor.spearman = cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,method="spearman")
cor.pearson = cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,method="pearson")

cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,
    method="spearman")
cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,
    method="pearson")
cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,
    method="kendall")


cor.test(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,)
cor.test(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,method="pearson",alternative="less")


cor(log(combined.df$IANA.Block.Count),
      log(combined.df$AlienVault.IANA.Count),method="spearman")
cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,
      method="spearman")


cor(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,method="spearman")
var(log(combined.df$IANA.Block.Count),log(combined.df$AlienVault.IANA.Count))
cov(combined.df$IANA.Block.Count,combined.df$AlienVault.IANA.Count,method="spearman")
cor.test(log(combined.df$IANA.Block.Count),log(combined.df$AlienVault.IANA.Count),method="spearman")


ggplot(data=combined.df) + 
  geom_point(aes(x=IANA.Block.Count, y=AlienVault.IANA.Count))
ggplot(data=combined.df) + 
  geom_point(aes(x=log(IANA.Block.Count),
                 y=log(AlienVault.IANA.Count)))
