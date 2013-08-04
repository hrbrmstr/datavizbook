library(RCurl)

#install.packages("igraph")
library(igraph)
library(plyr)

set.seed(1492)

# short function to trim leading/trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

BulkOrigin <- function(ip.list,host="v4.whois.cymru.com",port=43) {
  
  # Retrieves BGP Origin ASN info for a list of IP addresses
  #
  # NOTE: IPv4 version
  #
  # NOTE: The Team Cymru's service is NOT a GeoIP service!
  # Do not use this function for that as your results will not
  # be accurate.
  #
  # It also seems to fail if you only search for one IP address
  #
  # Args:
  #   ip.list : character vector of IP addresses
  #   host: which server to hit for lookup (defaults to Team Cymru)
  #   post: TCP port to use (defaults to 43)
  #
  # Returns:
  #   data frame of BGP Origin ASN lookup results
  
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query 
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) = c("AS","IP","BGP.Prefix","CC",
                      "Registry","Allocated","AS.Name")
  
  return(response)
  
}

BulkPeer <- function(ip.list,host="v4-peer.whois.cymru.com",port=43) {
  
  # Retrieves BGP Peer ASN info for a list of IP addresses
  #
  # NOTE: IPv4 version
  #
  # NOTE: The Team Cymru's service is NOT a GeoIP service!
  # Do not use this function for that as your results will not
  # be accurate.
  #
  # Args:
  #   ip.list : character vector of IP addresses
  #   host: which server to hit for lookup (defaults to Team Cymru)
  #   post: TCP port to use (defaults to 43)
  #
  # Returns:
  #   data frame of BGP Peer ASN lookup results
  
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  response <- laply(response,function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })  
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) <- c("Peer.AS","IP","BGP.Prefix","CC",
                       "Registry","Allocated","Peer.AS.Name")
  return(response)
  
}

BulkOriginASN <- function(asn.list,host="v4.whois.cymru.com",port=43) {
  
  # Retrieves BGP Origin ASN info for a list of ASN ids
  #
  # NOTE: prefix each ASN id with 'AS'
  #
  # NOTE: The Team Cymru's service is NOT a GeoIP service!
  # Do not use this function for that as your results will not
  # be accurate.
  #
  # Args:
  #   asn.list : character vector of ASN ids
  #   host: which server to hit for lookup (defaults to Team Cymru)
  #   post: TCP port to use (defaults to 43)
  #
  # Returns:
  #   data frame of BGP Origin ASN lookup results
  
  
  # setup query
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(asn.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  
  response <- response[2:length(response)]
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  
  response <- adply(response,c(1))
  response <- subset(response, select = -c(X1) )
  names(response) <- c("AS","CC","Registry","Allocated","AS.Name")
  
  return(response)
  
}

# retrieve ZeuS blocklist
zeusURL <- "https://zeustracker.abuse.ch/blocklist.php?download=ipblocklist"
zeusData <- "/suda/chapters/ch04/data/zeus.csv"
if (file.access(zeusData)) {
  # need to change download method for universal "https" compatibility
  download.file(zeusURL,zeusData,method="curl") 
}

# read in the ZeuS table; skip junk; no header; assign colnames
zeus <- read.table(zeusData,skip=5,header=FALSE,col.names=c("IP"))

# we'll be using the IP data quite a bit, so assign it to a variable
ips <- as.character(zeus$IP)

# get BGP origin data & peer data; we can use this data beyond
# making pretty graphs, too.
origin <- BulkOrigin(ips)
peers <- BulkPeer(ips)

# start graphing
g <- graph.empty()

# Make IP vertices; IP endpoints are red
g <- g + vertices(ips,size=2,color="red",group=1)

# Make BGP vertices
g <- g + vertices(
  grep("NA",unique(origin$AS),value=TRUE,invert=TRUE),
  size=2,color="orange", group=2)

# Make BGP peer vertices
g <- g + vertices(
  unique(c(peers$Peer.AS, origin$AS)),
  size=2,color="orange",group=2)

# Make IP/BGP edges
ip.edges <- lapply(ips,function(x) {
  iAS <- origin[origin$IP==x,]$AS
  lapply(iAS,function(y){
    c(x,y)
  })
})

# Make BGP/peer edges
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


# build ASN->IP edges
g <- g + edges(unlist(ip.edges))

# build ASN->Peer
g <- g + edges(unlist(bgp.edges))

# simplify the graph
g < simplify(g, edge.attr.comb=list(weight="sum"))

# delete any standalone vertices (lone wolf ASNs). In "graph" terms
# delete any vertex with a degree of 0
g <- delete.vertices(g, which(degree(g) < 1))

# we hates arrows
E(g)$arrow.size <- 0

# if we opt to show labels, we only care about ASN data, so
# blank out all the IP addresses 
V(g)[grep("\\.",V(g)$name)]$name <- ""


# this is a great layout for moderately sized networks. you can
# tweak the "n=10000" if this runs too slowly for you. The more
# iterations, the cleaner the graph will look
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)

# plot the graph
par(bg = 'white')
plot(g,margin=0,vertex.label=NA,layout=L, 
     main="ZeuS botnet nodes mapped across ASNs & ASN peers")

g = graph.empty()
g = g + vertices(ips,size=2,color="red",group=1)
g = g + vertices(
#  grep("NA",unique(origin$AS),value=TRUE,invert=TRUE),
  unique(origin$AS),
  size=2,color="orange", group=2)
ip.edges = lapply(ips,function(x) {
  iAS = origin[origin$IP==x,]$AS
  lapply(iAS,function(y){
    c(x,y)
  })
})
g = g + edges(unlist(ip.edges))
g = simplify(g, edge.attr.comb=list(weight="sum"))
g <- delete.vertices(g, which(degree(g) < 1))
E(g)$arrow.size <- 0
V(g)[grep("\\.",V(g)$name)]$name = ""
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
par(bg = 'white')
plot(g,
     margin=0,
     layout=L,
     vertex.label=NA,
     main="ZeuS botnet nodes; no ASN peers")

plot(g,margin=0,layout=L, 
     main="ZeuS botnet nodes; no ASN peers")

# the graph structure provides more than just a pretty picture; 
# we have a full data structure we can explore, starting with the
# degree of each vertex
deg = degree(g)
deg.ordered = order(deg)

# get the ASNs info out of the top 15
# "attributes" get us the 'names'
a = attributes(deg[tail(deg.ordered,15)])

# prepend "AS" to the list
mal = unlist(lapply(grep("NA",a$names,value=TRUE,invert=TRUE),
                    function(x) sprintf("AS%s",x)))
mal.detail = BulkOriginASN(list(mal))


# get top 15 bad ASNs
mal.df = data.frame(deg[tail(deg.ordered,7)])
mal.df$AS = rownames(mal.df)
rownames(mal.df) = NULL
colnames(mal.df) = c("count","AS")

# combine & sort data frames
badness = merge(mal.df, mal.detail, all.x=TRUE)
badness = arrange(badness,desc(count))
badness[,c(1,2,3,6)]

V(g)[badness$AS]$size = 5
plot(g,margin=0,layout=L,vertex.label=NA)




map <- function(x, range = c(0,1), from.range=NA) {
  if(any(is.na(from.range))) from.range <- range(x, na.rm=TRUE)
  
  ## check if all values are the same
  if(!diff(from.range)) return(
    matrix(mean(range), ncol=ncol(x), nrow=nrow(x), 
           dimnames = dimnames(x)))
  
  ## map to [0,1]
  x <- (x-from.range[1])
  x <- x/diff(from.range)
  ## handle single values
  if(diff(from.range) == 0) x <- 0 
  
  ## map from [0,1] to [range]
  if (range[1]>range[2]) x <- 1-x
  x <- x*(abs(diff(range))) + min(range)
  
  x[x<min(range) | x>max(range)] <- NA
  
  x
}


plot(g,vertex.label=NA,mark.groups=c$membership,vertex.size=map(v,c(1,5)))

c = walktrap.community(g, steps=10)
v <- evcent(g)$vector
par(mfrow=c(1,2))
par(mar=c(2,2,0,0))
plot(g,margin=0,vertex.label=NA,layout=L)
plot(c,g,
#      colbar=heat.colors(length(c)),
#      col=heat.colors(length(c))[membership(c)],
     vertex.label=NA,
     vertex.size=map(v,c(1,5)),
#     mark.groups=communities(c),
     layout=L)
par(mfrow=c(1,1))