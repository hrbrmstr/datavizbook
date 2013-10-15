# ch09-mds

# if you haven't installed verisr yet, do these two steps:
library(devtools)
# now let's install the verisr package
install_github("verisr", "jayjacobs")

# and load up the library
library(verisr)

# grab the incidents from the VCDB repository
# https://github.com/vz-risk/VCDB
# set the dir to the incidents/ directly of that
jsondir <- '../VCDB/incidents'
jsondir <- '../../../VCDB/incidents'
# create a veris instance with the vcdb data
vcdb <- json2veris(jsondir)

# convert veris object into a numeric matrix
vmat <- veris2matrix(vcdb)

# now pull the column names and extract industries
vmat.names <- colnames(vmat)
industry <- vmat.names[grep('victim.industry', vmat.names)]

# "fold" the matrix on industries
# this pulls all the incidents for the industry
# and compresses so the proportions of the features are represented.
imat <- foldmatrix(vmat, industry, min=10, clean=T)

#rownames(imat) <- txt.label

#idist <- dist(imat, 'canberra')
#pca <- prcomp(idist) #, scale=T, center=T)
# NO pca <- prcomp(imat, scale=T, center=T)
# create ggplot data.frame
#df <- data.frame(x=pca$x[ ,1], y=pca$x[ ,2], label=ind.label, size=ind.counts)

# convert the distance matrix
idist <- dist(imat, method='canberra')
# run it through classical MDS
cmd <- cmdscale(idist)
#plot(cmd)
# get the size of bubbles
ind.counts <- colSums(vmat[ , rownames(cmd)])
# extract the industry label
ind.label <- sapply(rownames(cmd), function(x) { 
  tail(unlist(strsplit(x, "[.]")), 1) 
})
# load up industry data
data(industry2)
# create a new list of short tet
txt.label <- industry2$short[which(industry2$code %in% ind.label)]


indf <- data.frame(x=cmd[ ,1], y=cmd[, 2], label=txt.label, size=ind.counts)
library(ggplot2)
gg <- ggplot(indf, aes(x, y, label=label, size=size))
gg <- gg + scale_size(trans="log2", range=c(10,30), guide=F)
gg <- gg + geom_point(fill="lightsteelblue", color="white", shape=21)
gg <- gg + xlim(range(df$x)*1.1) # expand x scale
gg <- gg + geom_text(size=4)
gg <- gg + theme(panel.grid = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank())
print(gg)
ggsave("figures/793725c06f003.eps", gg, width=8, height=5)

data(industry2)
df2 <- merge(df, industry2, by.x="label", by.y="code")

ggplot(df2, aes(x, y, label=short, size=size)) + 
  scale_size(trans="log2", range=c(10,30), guide=F) + 
  #scale_size(range=c(10,30), guide=F) + 
  geom_point(fill="lightsteelblue", color="black", shape=21) + 
  xlim(range(df$x)*1.04) +
  geom_text(size=4) + theme(panel.grid = element_blank(),
                            panel.border = element_blank(),
                            panel.background = element_blank(),
                            axis.text = element_blank(),
                            axis.title = element_blank(),
                            axis.ticks = element_blank())


# go back and relabel imat
rownames(imat) <- txt.label
# rerun idist
idist <- dist(imat, 'canberra')
# hclust couldn't be easier
hc <- hclust(idist) # , method="complete")
plot(hc)
setEPS()
postscript(file="figures/793725c06f004.eps", paper="special", 
           width=8, height=5, horizontal=FALSE) 
plot(hc)
dev.off()

# we can now cut off the heirarchical clustering at some level
# and use those levels to color the MDS plot
indf$cluster <- as.factor(cutree(hc, 5))
gg <- ggplot(indf, aes(x, y, label=label, size=size, fill=cluster))
gg <- gg + scale_size(trans="log2", range=c(10,30), guide=F)
gg <- gg + geom_point(color="gray80", shape=21)
gg <- gg + xlim(range(df$x)*1.04) # expand x scale
gg <- gg + geom_text(size=4)
gg <- gg + theme(panel.grid = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.text = element_blank(),
                 axis.title = element_blank(),
                 legend.position="none",
                 axis.ticks = element_blank())

print(gg)
ggsave("figures/793725c06f005.eps", gg, width=8, height=5)


### trying stuff
# vector of colors
mypal = c("#556270", "#4ECDC4", "#1B676B", "#FF6B6B", "#C44D58")
# cutting dendrogram in 5 clusters
clus5 = cutree(hc, 5)
# plot
op = par(bg="#E8DDCB")
# Size reflects miles per gallon
plot(as.phylo(hc), type="fan", tip.color=mypal[clus5], label.offset=1)
     #cex=log(mtcars$mpg,10), col="red")
par(op)

library(ggdendro)
ggdendrogram(hc, rotate=F, size=10, theme_dendro=T, color="tomato", segments=T)



#ok
## fix industry2
short <- c("Agriculture (11)", "Mining (21)", "Utilities(22)", "Construction (23)", 
           "Manufacturing (31)", "Manufacturing (32)", "Manufacturing (33)",
           "Trade (42)", "Retail (44)", "Retail(45)", "Transportation (48)", 
           "Transportation (49)", "Information (51)", "Finance (52)", 
           "Real Estate (53)", "Professional (54)", "Management(55)", 
           "Administrative (56)", "Educational (61)", "Healthcare (62)",
           "Entertainment (71)", "Accomodation (72)", "Other Services (81)",
           "Public (92)")






# now pull the column names and extract industries
vmat.names <- colnames(vmat)
industry <- vmat.names[grep('victim.industry', vmat.names)]
actor.action <- vmat.names[grep('^action|^actor', vmat.names)]
actor.action <- actor.action[grep('Unknown', actor.action, invert=T)]
vmat.trim <- vmat[ ,actor.action]

# now convert the matrix to be proportions within each industry
# test min
allnames <- colSums(vmat[ ,industry])
names(allnames)[which(allnames<3)]


foo <- foldmatrix(vmat, industry, min=10, clean=T)
vmat.names <- colnames(foo)
actor.action <- vmat.names
actor.action <- vmat.names[grep('^action|^actor|^asset', vmat.names)]
actor.action <- actor.action[grep('Unknown', actor.action, invert=T)]
foo2 <- foo[ ,actor.action]
#asdf <- apply(foo2, 2, var)


ind.counts <- colSums(vmat[ , rownames(foo)])
label <- sapply(rownames(foo), function(x) { tail(unlist(strsplit(x, "[.]")), 1) })

pca <- prcomp(dist(foo, 'canberra'), scale=T, center=T, method="euclidian")
df <- data.frame(x=pca$x[ ,1], y=pca$x[ ,2], label=label, size=ind.counts)

# this is the winner.
cmd <- cmdscale(dist(foo, 'canberra'))
df <- data.frame(x=cmd[ ,1], y=cmd[, 2], label=label, size=ind.counts)
ggplot(df, aes(x, y, label=label, size=size)) + 
  #scale_size(trans="log10", range=c(10,30), guide=F) + 
  scale_size(range=c(10,30), guide=F) + 
  geom_point(fill="lightsteelblue", color="black", shape=21) + 
  geom_text(size=4)

data(industry2)


# plot(cmd[ ,1], cmd[, 2], type="n")
#text(cmd[ ,1], cmd[ ,2], label)
