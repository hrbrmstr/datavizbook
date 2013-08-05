CODE FOR FIGURE 4.13

origin <- BulkOrigin(ips)
peers <- BulkPeer(ips)
g <- graph.empty()
g <- g + vertices(ips, size=2, color="red", group=1)
g <- g + vertices(unique(c(peers$Peer.AS, origin$AS)),
                  size=2,color="orange",group=2)
ip.edges <- lapply(ips,function(x) {
  iAS <- origin[origin$IP==x,]$AS
  lapply(iAS,function(y){
    c(x,y)
  })
})
bgp.edges <- lapply(
  grep("NA",unique(origin$BGP.Prefix),value=TRUE,invert=TRUE),
  function(x) {
    startAS <- unique(origin[origin$BGP.Prefix==x,]$AS)
    lapply(startAS,function(z) {
      pAS <- peers[peers$BGP.Prefix==x,]$Peer.AS
      lapply(pAS,function(y) {
        c(z,y)
      })
    })
  })
g <- g + edges(unlist(ip.edges))
g <- g + edges(unlist(bgp.edges))
g <- delete.vertices(g, which(degree(g) < 1))
g <- simplify(g, edge.attr.comb=list(weight="sum"))
E(g)$arrow.size <- 0
V(g)[grep("\\.",V(g)$name)]$name = ""
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
par(bg = 'white')
plot(g,margin=0,layout=L,vertex.label.dist=0.5, 
     vertex.label=NA,
     main="ZeuS botnet nodes mapped across ASNs & ASN peers")
