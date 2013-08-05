# CODE FOR FIGURE 4.12

g <- graph.empty()
g <- g + vertices(ips, size=2, color="red", group=1)
g <- g + vertices(unique(origin$AS),size=2,color="orange",group=2)
ip.edges <- lapply(ips,function(x) {
  iAS <- origin[origin$IP==x,]$AS
  lapply(iAS,function(y){
    c(x,y)
  })
})
g <- g + edges(unlist(ip.edges))
g <- delete.vertices(g, which(degree(g) < 1))
g <- simplify(g, edge.attr.comb=list(weight="sum"))
E(g)$arrow.size <- 0
V(g)[grep("\\.",V(g)$name)]$name = ""
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
par(bg = 'white')
plot(g,margin=0,layout=L,vertex.label.dist=0.5, 
     vertex.label=NA,
     main="ZeuS botnet nodes; no ASN peers")