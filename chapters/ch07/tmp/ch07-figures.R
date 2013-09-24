# if we haven't installed devtools yet, let's install it
library(devtools)

# now let's install the verisr package
install_github("verisr", "jayjacobs")

# and load up the library
library(verisr)

# grab the incidents from the VCDB repository
# https://github.com/vz-risk/VCDB
# set the dir to the incidents/ directly of that
jsondir <- '../../../../github/VCDB/incidents'

# create a veris instance with the vcdb data
vcdb <- json2veris(jsondir)

# we can get some high level overview of the incidents by running
# (uncomment if you want to run)
summary(vcdb)
plot(vcdb)

# get an enum by actor types and action types
# create a contingency table 

actors <- getenum(vcdb, "actor")
print(actors)
##      enum    x
## 1  unknown   86
## 2  partner  104
## 3 internal  556
## 4 external 1024

actors <- getenum(vcdb, "actor", add.n=TRUE, add.freq=TRUE)
print(actors)
##       enum    x    n freq
## 1  unknown   86 1737 0.05
## 2  partner  104 1737 0.06
## 3 internal  556 1737 0.32
## 4 external 1024 1737 0.59

a.variety <- getenum(vcdb, "actor.external.variety", add.n=T, add.freq=T)
a.variety$x[a.variety$enum=="Unknown"] <- 619+220
a.variety <- a.variety[-(which(a.variety$enum=="Unaffiliated")), ]
a.variety$freq <- round(a.variety$x/a.variety$n, 2)
gg <- ggplot(a.variety, aes(x=enum, y=x)) + geom_bar(stat="identity")
print(gg)

########
## actor section
#######
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
actors <- getenumlist(vcdb, "actor")
actfields <- c("external", "internal", "partner")
actors.n <- sum(sapply(actors, function(x) {
  any(actfields %in% x)
}))
motive <- getenum(vcdb, "actor.external.motive")
motive <- rbind(motive, getenum(vcdb, "actor.internal.motive"))
motive <- rbind(motive, getenum(vcdb, "actor.partner.motive"))
allmotive <- aggregate(x ~ enum, data=motive, FUN=sum)
allmotive <- allmotive[with(allmotive, order(x)), ]
allmotive$enum <- factor(allmotive$enum, levels=allmotive$enum, ordered=T)
allmotive$freq <- allmotive$x/actors.n
## remove unknown as a proportion
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown" | allmotive$enum=="NA"), ]
finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(finalmotive, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(allmotive$freq)+.02)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)
ggsave("figures/793725c07f001.pdf", gg, height=6, width=8)

########
## actor section
#######
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
actions <- getenumlist(vcdb, "action")
actfields <- c("malware", "hacking", "social", "error", "misuse", "physical") # sorry env
actions.n <- sum(sapply(actions, function(x) {
  any(actfields %in% x)
}))
# falling in love with "do.call"
action <- do.call(rbind, lapply(actfields, function(a) {
  rawaction <- getenum(vcdb, paste('action.', a, '.variety', sep=''))
  rawaction$enum <- paste(rawaction$enum, ' (', substr(a, 1, 3) ,')', sep='')
  rawaction
}))
allaction <- action[with(action, order(-x)), ]
# gotta do a top 5
topaction <- allaction[which(allaction$enum %in% allaction$enum[1:10]), ]
topaction$enum <- factor(topaction$enum, levels=rev(topaction$enum), ordered=T)
topaction$freq <- topaction$x/actions.n
## remove unknown as a proportion
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown" | allmotive$enum=="NA"), ]
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(topaction, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(topaction$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)
ggsave("figures/793725c07f002.pdf", gg, height=6, width=8)

########
## actor section
#######
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
# this is easy compared to the rest!
asset <- getenum(vcdb, "asset.assets", add.freq=T)

# flip it
asset <- asset[(with(asset, order(x))), ]
asset$enum <- factor(asset$enum, levels=asset$enum, ordered=T)
#levels(asset$enum) <- rev(levels(asset$enum))

gg <- ggplot(asset, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(asset$freq)+.03)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)
ggsave("figures/793725c07f003.pdf", gg, height=6, width=8)

## this is a bit of a hack, as verisr does not do industry well yet.
########
## actor section
#######
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
data("industry2")  # part of verisr package
rawi <- getenum(vcdb, "victim.industry2", add.n=T)
industrytext <- merge(rawi, industry2, by.x="enum", by.y="code", all.x=T)
aggind <- aggregate(x ~ title + n, data=industrytext, FUN=sum)
# flip it
industry <- aggind[(with(aggind, order(x))), ]
industry <- industry[c((nrow(industry)-4):nrow(industry)), ]
# hack until I fix industry2 data set with short title
industry$title <- as.character(industry$title)
#industry$title[which(industry$title=="Professional, Scientific, and Technical Services")] <- "Professional Services"
industry$title[which(industry$title=="Health Care and Social Assistance")] <- "Health Care"
industry$title <- factor(industry$title, levels=industry$title, ordered=T)
industry$freq <- industry$x/industry$n
gg <- ggplot(industry, aes(x=title, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(industry$freq)+.05)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)
ggsave("figures/793725c07f004.pdf", gg, height=6, width=10)


## what if I make an 2 x 3 facet with 6 different "getenum" calls
library(ggplot2)

# take in the vcdb object and the field to plot
verisplot <- function(vcdb, field) {
  # get the data.frame for the field with frequency
  localdf <- getenum(vcdb, field, add.freq=T)
  # now let's take first 5 fields in the data frame.
  localdf <- localdf[c(1:5), ]
  # add a label to the data.frame
  localdf$lab <- paste(round(localdf$freq*100, 0), "%", sep="")
  # now we can create a ggplot2 instance
  gg <- ggplot(localdf, aes(x=enum, y=freq, label=lab))
  gg <- gg + geom_bar(stat="identity", fill="steelblue")
  # add in text, adjusted to the end of the bar
  gg <- gg + geom_text(hjust=-0.1)
  # flip the axes and add in a title
  gg <- gg + coord_flip() + ggtitle(field)
  # remove axes labels and add bw theme
  gg <- gg + xlab("") + ylab("") + theme_bw()
  # fix the y scale to remove padding and fit our label (add 7%) 
  gg <- gg + scale_y_continuous(expand=c(0,0), limits=c(0, max(localdf$freq)*1.07))
  # make it slightly prettier than the default
  gg <- gg + theme(panel.grid.major = element_blank(),
                   panel.border = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks = element_blank())
}
print(verisplot(vcdb, "action"))
#library(scales)
library(grid)
library(gridExtra)

plotenum <- function(exec.call) {
  enum.df <- getenum(vcdb, exec.call, add.freq=T)[1:5, ]
  gg <- ggplot(enum.df, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
  gg <- gg + geom_bar(stat="identity", fill="steelblue") 
  gg <- gg + geom_text(hjust=-0.1, size=2)
  gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(enum.df$freq)+.09)) 
  gg <- gg + coord_flip()
  gg <- gg + xlab("") + ylab("") + ggtitle(paste('"', exec.call, '"', sep=""))
  gg <- gg + theme(axis.text.x = element_blank(),
                   text = element_text(size=7, color="black"),
                   axis.text = element_text(size=7, color="black"),
                   axis.ticks = element_blank(),
                   plot.background = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank())
  gg
}

a1 <- plotenum("actor.external.variety")
a2 <- plotenum("action")
b1 <- plotenum("action.physical.variety")
b2 <- plotenum("action.hacking.vector")
c1 <- plotenum("attribute.confidentiality.data.variety")
c2 <- plotenum("asset.assets")
pdf("figures/793725c07f005.pdf", width=6, height=6)
grid.arrange(a1, a2, b1, b2, c1, c2, ncol=2, clip=T)
dev.off()

####
## A2 Grid
####

# this will add zero's in missing squares
a2 <- getenum(vcdb, "action", primary="asset.assets", add.freq=T)
# trim unknown asset and environment action for space
a2 <- a2[-which(a2$enum=="environmental" | a2$primary=="Unknown"), ]

# this code is not the book to save space, but it orders the factors
assetlvl <- c("Server", "Network", "User Dev", "Media", 
              "Person", "Kiosk/Term")
actionlvl <- c("malware", "hacking", "social", 
               "misuse", "physical", "error")
a2$primary <- factor(a2$primary, levels=rev(assetlvl), ordered=T)
a2$enum <- factor(a2$enum, levels=actionlvl, ordered=T)

# so we should create a "slim" version without zeros to color it
slim.a2 <- a2[-which(a2$x==0), ]
  
gg <- ggplot(a2, aes(x=enum, y=primary, fill=freq))
gg <- gg + geom_tile(fill="white", color="gray80")
gg <- gg + geom_tile(data=slim.a2, color="gray80")
gg <- gg + scale_fill_gradient(low = "#F0F6FF", 
                               high = "#4682B4", guide=F)
gg <- gg + xlab("") + ylab("") + theme_bw()
gg <- gg + scale_x_discrete(expand=c(0,0))
gg <- gg + scale_y_discrete(expand=c(0,0))
gg <- gg + theme(axis.ticks = element_blank())
print(gg)
ggsave("figures/793725c07f006.pdf", gg, height=5, width=5)




vzdir <- c("~/Documents/json/newfinal/afp",
           "~/Documents/json/newfinal/ias1",
           "~/Documents/json/newfinal/ias2",
           "~/Documents/json/newfinal/ias3",
           "~/Documents/json/newfinal/legacy",
           "~/Documents/json/newfinal/ss",
           "~/Documents/json/newfinal/ss2",
           "~/Documents/json/newfinal/ss3",
#           "~/Documents/json/newfinal/mal",
#           "~/Documents/json/newfinal/uscert",
           "~/Documents/json/newfinal/vzint",
           "~/Documents/json/newfinal/vzir")
vz <- json2veris(vzdir)
names(vz) <- NULL

old.prepa2 <- function(veris, filter, src, flabel) {
  a2 <- getenum(veris, "action", primary="asset.assets", filter=filter, add.freq=T)
  # trim unknown asset and environment action for space
  a2 <- a2[which(a2$enum!="environmental"), ]
  a2 <- a2[which(a2$enum!="unknown"), ]
  a2 <- a2[which(a2$primary!="Unknown"), ]
  # overwrite the default freq with a scaled version
  a2$freq <- a2$freq/max(a2$freq)
  a2$src <- src
  a2$flabel <- flabel
  a2
}

prepa2 <- function(veris, filter, src, flabel) {
  #flabel <- ifelse (flabel=="DBIR", "DBIR (2011-2013)"
  assets <- getenumlist(veris, "asset.assets")
  actions <- getenumlist(veris, "action")
  filter.asset <- ifelse(filter, assets, NA)
  filter.action <- ifelse(filter, actions, NA)
  full.df <- do.call(rbind, lapply(seq(length(filter.asset)), function(index) {
    # skip if we have any NA values across the board.
    if (!any(is.na(c(filter.asset[[index]], filter.action[[index]])))) {
      expand.grid(enum=unique(unlist(filter.action[[index]])),
                  primary=unique(unlist(filter.asset[[index]])))
    }
  }))
  count <- sum(sapply(seq_along(filter.asset), function(x) {
    all(c(!is.na(filter.asset[x]), !is.na(filter.action[x])))
  }))
  full.df$x <- 1
  retval <- aggregate(x ~ ., data=full.df, FUN=sum)
#  print(retval)
  retval$freq <- retval$x/count
  retval$freq <- retval$freq/max(retval$freq)
  retval$src <- src
  retval$flabel <- flabel
  retval
}



notenv <- !getfilter(vcdb, list("action"="environmental"))
notunk.act <- !getfilter(vcdb, list("action"="unknown"))
notunk.ass <- !getfilter(vcdb, list("asset.assets"="unknown"))
baseunk.vcdb <- sapply(seq_along(notenv), function(x) { 
  all(c(notenv[x], notunk.act[x], notunk.ass[x]))
})
notenv <- !getfilter(vz, list("action"="environmental"))
notunk.act <- !getfilter(vz, list("action"="unknown"))
notunk.ass <- !getfilter(vz, list("asset.assets"="Unknown"))
baseunk.vz <- sapply(seq_along(notenv), function(x) { 
  all(c(notenv[x], notunk.act[x], notunk.ass[x]))
})


fdf <- prepa2(vcdb, filter=baseunk.vcdb, src="VCDB", flabel="All Events")
fdf <- rbind(fdf, prepa2(vz, filter=baseunk.vz, src="DBIR", flabel="All Events"))

justdd <- list("attribute.confidentiality.data_disclosure" = "Yes")
vcdb.dd <- getfilter(vcdb, and=justdd)
vcdb.dd <- sapply(seq_along(vcdb.dd), function(x) {
  all(c(baseunk.vcdb[x], vcdb.dd[x]))
})

just2011 <- getfilter(vz, list("plus.dbir_year"=2011))
just2012 <- getfilter(vz, list("plus.dbir_year"=2012))
just2013 <- getfilter(vz, list("plus.dbir_year"=2013))
justyear <- sapply(seq_along(just2011), function(x) { 
  any(c(just2011[x], just2012[x], just2013[x]))
})
  
vz.dd <- getfilter(vz, and=justdd)
vz.base <- sapply(seq_along(vz.dd), function(x) { 
  all(c(justyear[x], vz.dd[x], baseunk.vz[x])) })

fdf <- rbind(fdf, prepa2(vcdb, filter=vcdb.dd, src="VCDB", flabel="Confirmed Data Loss"))
fdf <- rbind(fdf, prepa2(vz, filter=vz.base, src="DBIR", flabel="Confirmed Data Loss"))

just2011 <- getfilter(vcdb, list("actor.external.motive"="Financial"))
just2012 <- getfilter(vcdb, list("actor.external.motive"="Financial"))
just2013 <- getfilter(vcdb, list("actor.external.motive"="Financial"))
vcdb.fina <- sapply(seq_along(just2011), function(x) { 
  any(c(just2011[x], just2012[x], just2013[x]))
})
just2011 <- getfilter(vz, list("actor.external.motive"="Financial"))
just2012 <- getfilter(vz, list("actor.external.motive"="Financial"))
just2013 <- getfilter(vz, list("actor.external.motive"="Financial"))
vz.fina <- sapply(seq_along(just2011), function(x) { 
  any(c(just2011[x], just2012[x], just2013[x]))
})
vcdb.fin <- sapply(seq_along(vcdb.fina), function(x) { 
  all(c(vcdb.dd[x], vcdb.fina[x])) })

vzfin.real <- sapply(seq_along(vz.fina), function(x) {
  all(c(vz.fina[x], vz.base[x]))
})

fdf <- rbind(fdf, prepa2(vcdb, filter=vcdb.fin, src="VCDB", flabel="Financial Motives"))
fdf <- rbind(fdf, prepa2(vz, filter=vzfin.real, src="DBIR", flabel="Financial Motives"))

aa <- getfilter(vz, list("actor.external.motive"="Fun"))
ab <- getfilter(vz, list("actor.internal.motive"="Fun"))
ac <- getfilter(vz, list("actor.partner.motive"="Fun"))
ad <- getfilter(vz, list("actor.external.motive"="Ideology"))
ae <- getfilter(vz, list("actor.internal.motive"="Ideology"))
af <- getfilter(vz, list("actor.partner.motive"="Ideology"))
vz.first <- sapply(seq_along(aa), function(x) {
  any(c(aa[x], ab[x], ac[x], ad[x], ae[x], af[x])) })

aa <- getfilter(vcdb, list("actor.external.motive"="Fun"))
ab <- getfilter(vcdb, list("actor.internal.motive"="Fun"))
ac <- getfilter(vcdb, list("actor.partner.motive"="Fun"))
ad <- getfilter(vcdb, list("actor.external.motive"="Ideology"))
ae <- getfilter(vcdb, list("actor.internal.motive"="Ideology"))
af <- getfilter(vcdb, list("actor.partner.motive"="Ideology"))
vcdb.first <- sapply(seq_along(aa), function(x) {
  any(c(aa[x], ab[x], ac[x], ad[x], ae[x], af[x])) })

vcdb.act <- sapply(seq_along(vcdb.first), function(x) { 
  all(c(vcdb.dd[x], vcdb.first[x])) })

vz.act <- sapply(seq_along(vz.fina), function(x) {
  all(c(vz.first[x], vz.base[x]))
})
    
fdf <- rbind(fdf, prepa2(vcdb, filter=vcdb.act, src="VCDB", flabel="Ideology or Fun"))
fdf <- rbind(fdf, prepa2(vz, filter=vz.act, src="DBIR", flabel="Ideology or Fun"))

fdf <- fdf[which(fdf$primary!="Unknown"), ]
savefdf <- fdf
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
#capwords(c("malware", "hacking"))
fdf$enum <- capwords(as.character(fdf$enum))
assetlvl <- c("Server", "Network", "User Dev", "Media", 
              "Person", "Kiosk/Term")
actionlvl <- c("Malware", "Hacking", "Social", 
               "Misuse", "Physical", "Error")

fdf$primary <- factor(fdf$primary, levels=rev(assetlvl), ordered=T)
fdf$enum <- factor(fdf$enum, levels=actionlvl, ordered=T)
fdf$freq <- round(fdf$freq, 2)
white.df <- expand.grid(enum=unique(as.character(fdf$enum)), primary=unique(as.character(fdf$primary)),
                        src=unique(as.character(fdf$src)), flabel=unique(as.character(fdf$flabel)))
white.df$x <- 0
white.df$freq <- 0
white.df$primary <- factor(white.df$primary, levels=rev(assetlvl), ordered=T)
white.df$enum <- factor(white.df$enum, levels=actionlvl, ordered=T)



gg <- ggplot(white.df, aes(x=enum, y=primary, fill=freq))
gg <- gg + geom_tile(fill="white", color="gray80")
gg <- gg + geom_tile(data=fdf, color="gray80")
gg <- gg + facet_grid(flabel ~ src)
gg <- gg + scale_fill_gradient(low = "#FFFFFF", 
                               high = "#4682B4", guide=F)
gg <- gg + xlab("") + ylab("") + theme_bw()
gg <- gg + scale_x_discrete(expand=c(0,0))
gg <- gg + scale_y_discrete(expand=c(0,0))
gg <- gg + theme(axis.ticks = element_blank(), 
                 axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
                 strip.text.y = element_text(angle=90),
                 strip.background=element_blank())

print(gg)
ggsave("figures/793725c07f007.pdf", gg, height=7, width=5)


basefilter <- list("action"="environment", "asset.assets"="Unknown")
base <- getfilter(vcdb, or.not=basefilter)

justdd <- list("attribute.confidentiality.data_disclosure" = "Yes")
# sort it right
assetlvl <- c("Server", "Network", "User Dev", "Media", 
              "Person", "Kiosk/Term")
actionlvl <- c("malware", "hacking", "social", 
               "misuse", "physical", "error")
fdf$primary <- factor(fdf$primary, levels=rev(assetlvl), ordered=T)
fdf$enum <- factor(fdf$enum, levels=actionlvl, ordered=T)

slim.a2 <- fdf[-which(fdf$x==0), ]
#white.df <- expand.grid(enum=unique(as.character(fdf$enum)), primary=unique(as.character(fdf$primary)),
#                        src=unique(as.character(fdf$src)), flabel=unique(as.character(fdf$flabel)))
#white.df$x <- 0
#white.df$freq <- 0
  gg <- ggplot(fdf, aes(x=enum, y=primary, fill=freq))
  gg <- gg + geom_tile(fill="white", color="gray80")
  gg <- gg + geom_tile(data=slim.a2, color="gray80")
  gg <- gg + scale_fill_gradient(low = "#F0F6FF", 
                                 high = "#4682B4", guide=F)
  gg <- gg + xlab("") + ylab("") + theme_bw()
  gg <- gg + scale_x_discrete(expand=c(0,0))
  gg <- gg + scale_y_discrete(expand=c(0,0))
  gg <- gg + theme(axis.ticks = element_blank())
  
}







a.variety <- getenum(vcdb, "actor.external.variety", add.n=T, add.freq=T)
a.variety$x[a.variety$enum=="Unknown"] <- 619+220
a.variety <- a.variety[-(which(a.variety$enum=="Unaffiliated")), ]
a.variety$freq <- round(a.variety$x/a.variety$n, 2)
gg <- ggplot(a.variety, aes(x=enum, y=x)) + geom_bar(stat="identity")
print(gg)

########
## actor section
#######
library(ggplot2)
library(scales)
# should have verisr loaded and vcdb object created
actors <- getenumlist(vcdb, "actor")
actfields <- c("external", "internal", "partner")
actors.n <- sum(sapply(actors, function(x) {
  any(actfields %in% x)
}))
motive <- getenum(vcdb, "actor.external.motive")
motive <- rbind(motive, getenum(vcdb, "actor.internal.motive"))
motive <- rbind(motive, getenum(vcdb, "actor.partner.motive"))
allmotive <- aggregate(x ~ enum, data=motive, FUN=sum)
allmotive <- allmotive[with(allmotive, order(x)), ]
allmotive$enum <- factor(allmotive$enum, levels=allmotive$enum, ordered=T)
allmotive$freq <- allmotive$x/actors.n
## remove unknown as a proportion
#finalmotive <- allmotive[-which(allmotive$enum=="Unknown" | allmotive$enum=="NA"), ]
finalmotive <- allmotive[-which(allmotive$enum=="Unknown"), ]
gg <- ggplot(finalmotive, aes(x=enum, y=freq, label=paste(round(freq, 2)*100, "%", sep='')))
gg <- gg + geom_bar(stat="identity", fill="steelblue") 
gg <- gg + geom_text(hjust=-0.1)
gg <- gg + scale_y_continuous(labels=percent, expand=c(0,0), limits=c(0, max(allmotive$freq)+.02)) 
gg <- gg + coord_flip()
gg <- gg + xlab("") + ylab("") # title in book caption
gg <- gg + theme(axis.text.x = element_blank(),
                 axis.text = element_text(size=14, color="black"),
                 axis.ticks = element_blank(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
print(gg)
ggsave("figures/793725c07f001.pdf", gg, height=6, width=8)