library(igraph)
origin <- BulkOrigin(ips)
g <- graph.empty()
g <- g + vertices(ips,size=2,color="red",group=1)
g <- g + vertices(
  unique(origin$CC),
  size=2,color="orange",group=2)
ip.cc.edges <- lapply(ips,function(x) {
  iCC <- origin[origin$IP %in% x,]$CC
  lapply(iCC,function(y){
    c(x,y)
  })
})
g <- g + edges(unlist(ip.cc.edges))
g <- delete.vertices(g, which(degree(g) < 1))
E(g)$arrow.size <- 0
V(g)[grep("\\.",V(g)$name)]$name = ""
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
par(bg = 'white')
plot(g,margin=0,layout=L,vertex.label.dist=0.5, 
     vertex.label.cex=0.75, 
     vertex.label.color="black",
     vertex.label.family="sans", 
     vertex.label.font=2,
     main="ZeuS botnet nodes clustered by country")

# as of the time writing, the 'countrycodes' package by
# Vincent Arel-Bundock (@VincentAB) hasn't been updated for R 3, so
# we'll 'borrow' the CSV file he used to make the cc->name
# translation ourself to extract the country code vertices from
# the graph and alpha-order them
zeus.cc = grep("[A-Z]",V(g)$name,value=TRUE)
zeus.cc = zeus.cc[order(zeus.cc)]
# read in the country codes data frame
cc.df = read.csv("/suda/chapters/ch04/data/countrycode_data.csv")
# display cc & name for just the ones from our data set
print(head(cc.df[cc.df$iso2c %in% zeus.cc,c(7,1)],n=10),
      row.names=FALSE)

c <- walktrap.community(g, steps=10)
v <- evcent(g)$vector
opar = par()
par(mar=c(0,0,0,0))
plot(g,margin=0,layout=L,vertex.label.dist=0.5, 
     vertex.label.cex=0.75, 
     vertex.label.color="black",
     vertex.label.family="sans", 
     vertex.label.font=2,
     mark.groups=communities(c),
     mark.col=c("gray100"),
     mark.border=c("black"))
par = opar

#pal(sequential_hcl(12, c = 0, power = 2.2))

ggplot(data=origin) +
  geom_bar(aes(CC)) + # sums count of each value for bar height 
  coord_flip() + # swap x & y axes
  labs(title="ZeuS Botnet Count By Country")