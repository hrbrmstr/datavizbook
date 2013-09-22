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

