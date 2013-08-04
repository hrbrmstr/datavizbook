library(igraph)
library(plyr)
library(bitops)
library(colorspace)

ip2long <- function(ip) {
  # Reduce applys a function cumulatively on the arguments
  # from left to right. bit-shift, then "OR" the octets
  Reduce(function(x, y) { 
    bitOr(bitShiftL(x,8),y)
  }, sapply(strsplit(ip,".",fixed=TRUE),function(o) {
    as.integer(o)
  }))
}

long2ip <- function(longip) {
  # paste converts arguments to character andconcatenates them
  # Map applys a function to each element of the arguent
  paste(Map(function(nbits) { 
    bitAnd(bitShiftR(longip,nbits),0xFF)
  }, c(24,16,8,0)), sep="",collapse=".")
}

is.ip <- function(ip) {
  return(grepl("^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$",ip))
}

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

graph.cc <- function(ips,alien.vault.df,show.plot=TRUE) {
  
  # Lookup ASN info for the incoming IP list which will
  # have country of origin info that's fairly accurate
  origin <- BulkOrigin(ips)
  
  # filter out IP and Type from the alienvault DB only for our ip list
  ips.types <- alien.vault.df[alien.vault.df$IP %in% ips,c(1,2,4)]
  
  # get a tabular summary of the types and counts
  ftab <- table(factor(ips.types[ips.types$IP %in% ips,]$Type))
  
  # build a color table from the tabular summary
  myColors <- rainbow_hcl(length(names(ftab)),c=60,l=70,start=20)
  col.df <- data.frame(Type=names(ftab),Color=myColors)
  
  # begin graph creation
  g <- graph.empty()
  
  # add our ip list as the starting vertices
  g <- g + vertices(ips,size=3,group=1)
  
  # i don't the df is necessary anymore...will test later
  ips.df <- data.frame(ips)
  colnames(ips.df) <- c("IP")
  
  # get the current list of vertex names...i think i can remove this too
  v.names <- V(g)$name
  
  # assign colors to the vertices based on the type
  V(g)$color <- as.character(col.df[col.df$Type %in% ips.types[ips.types$IP %in% v.names,]$Type,]$Color)
  
  # add country vertices
  g <- g + vertices(
    grep("NA",unique(origin$CC),value=TRUE,invert=TRUE),
    size=2,color="orange",group=2)

  # build country->ip edges
  ip.cc.edges <- lapply(ips,function(x) {
    iCC <- origin[origin$IP==x,]$CC
    lapply(iCC,function(y){
      c(x,y)
    })
  })

  # add edges
  g <- g + edges(unlist(ip.cc.edges))

  # simplify (though it's almost not necessary given the low
  # complexity of the graph)
  g <- simplify(g, edge.attr.comb=list(weight="sum"))

  # remove lone wolf vertices
  g <- delete.vertices(g, which(degree(g) < 1))

  # arrows: ugh
  E(g)$arrow.size <- 0

  # we only want to see country labels, not IPs
  V(g)[grep("[0-9]",V(g)$name)]$name <- ""

  # 10000 makes pretty graphs and takes a pretty long time
  L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)

  # for when I add community options
  c <- walktrap.community(g, steps=10)
  v <- evcent(g)$vector
  
  if (show.plot) {
    def.par <- par(no.readonly = TRUE)
    par(bg = 'white')
    layout(matrix(c(1,2,3,1), 1, 2, byrow = TRUE), widths=c(5,1))
    plot(g,margin=0,layout=L,vertex.label.dist=0.6, 
         vertex.label.cex=0.75, 
         vertex.label.color="black",
         vertex.label.family="sans", 
         vertex.label.font=2)
    par(mar=c(5,0,2,2))
    barplot(ftab,horiz=TRUE,las=1,cex.names=0.75,cex.axis=0.75,
            col=as.character(col.df[col.df$Type %in% unlist(labels(ftab)),]$Color))
    par(def.par)
  }
  return(g)
}

graph.asn <- function(ips,alien.vault.df,add.peers=FALSE,show.plot=TRUE,show.labels=FALSE) {
  
  # Lookup ASN info for the incoming IP list
  origin <- BulkOrigin(ips)

  if (add.peers) { # and peers if specified
    peers <- BulkPeer(ips)
  }
  
  # filter out IP and Type from the alienvault DB only for our ip list
  ips.types <- alien.vault.df[alien.vault.df$IP %in% ips,c(1,2,4)]
  
  # get a tabular summary of the types and counts
  ftab <- table(factor(ips.types[ips.types$IP %in% ips,]$Type))

  # build a color table from the tabular summary
  myColors <- rainbow_hcl(length(names(ftab)),c=60,l=70,start=20)
  col.df <- data.frame(Type=names(ftab),Color=myColors)
  
  # begin graph creation
  g <- graph.empty()

  # add our ip list as the starting vertices
  g <- g + vertices(ips,size=3,group=1)
  
  # i don't think the df is necessary anymore...will test later
  ips.df <- data.frame(ips)
  colnames(ips.df) <- c("IP")
  
  # get the current list of vertex names...i think i can remove this too
  v.names <- V(g)$name

  # assign colors to the vertices based on the type
  V(g)$color <- as.character(col.df[col.df$Type %in% ips.types[ips.types$IP %in% v.names,]$Type,]$Color)
  
  # now make primary BGP/ASN vertices
  g <- g + vertices(
    grep("NA",unique(origin$AS),value=TRUE,invert=TRUE),
    size=2,color="orange", group=2)

  if (add.peers) { # same for peers if specified
    g <- g + vertices(
      unique(c(peers$Peer.AS, origin$AS)),
      size=2,color="orange",group=2)
  }
  
  # Make IP/BGP edges
  ip.edges <- lapply(ips,function(x) {
    iAS <- origin[origin$IP==x,]$AS
    lapply(iAS,function(y){
      c(x,y)
    })
  })
  
  if (add.peers) { # same for peers if specified
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
  }
  
  # build ASN->IP edges
  g <- g + edges(unlist(ip.edges))
  
  if (add.peers) { # same for peers if specified
    g <- g + edges(unlist(bgp.edges))
  }
  
  # simplify the structure (prbly needed since it's already
  # well organized w/o dupes
  g <- simplify(g, edge.attr.comb=list(weight="sum"))
  
  # delete any standalone vertices (lone wolf ASNs)
  g <- delete.vertices(g, which(degree(g) < 1))
  
  # arrows: ugh
  E(g)$arrow.size <- 0
  
  # if we do show labels, we only want to see the ASNs
  V(g)[grep("\\.",V(g)$name)]$name <- ""

  # 10000 makes it pretty...and pretty slow
  L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
  
  # shld make an options parameter and if-block this
  c <- walktrap.community(g, steps=10)
  v <- evcent(g)$vector

  if (show.plot) {
    def.par <- par(no.readonly = TRUE)
    par(bg = 'white')
    layout(matrix(c(1,2,3,1), 1, 2, byrow = TRUE),
           widths=c(5,1))
    if (show.labels) {
      plot(g,
           margin=0,
           layout=L,
           vertex.label.dist=0.6, 
           vertex.label.cex=0.75, 
           vertex.label.color="black",
           vertex.label.family="sans", 
           vertex.label.font=2)
    } else {
      plot(g,margin=0,vertex.label=NA,layout=L)      
    }
    par(mar=c(5,0,2,2))
    barplot(ftab,horiz=TRUE,las=1,cex.names=0.75,cex.axis=0.75,
            col=as.character(
              col.df[col.df$Type %in% unlist(labels(ftab)),]$Color))
    par(def.par)
  }
  return(g)
}

set.seed(1492)

# read in the alien vault database
avRep <- "~/Dropbox/datavizbook/chapters/ch03/data/reputation.data"
av.df <- read.csv(avRep,sep="#",header=FALSE)
colnames(av.df) <- c("IP","Reliability","Risk","Type",
                     "Country","Locale","Coords","x")


# load 1 day's worth of 1 firewall's recorded destination IP addresses
dest.ips <- read.csv("~/Desktop/dest.ips")
colnames(dest.ips) <- c("IP")

# from all our IP destinations, just extract the possible malicious ones
ips.types <- av.df[av.df$IP %in% dest.ips$IP,c(1,2,4)]

# filter out only the ones with a really bad reputation
#ips <- as.character(ips.types[ips.types$Reliability > 6,]$IP)
ips <- as.character(ips.types[ips.types$Reliability > 5,]$IP)

cc.g <- graph.cc(ips,av.df)
asn.g <- graph.asn(ips,av.df)
asn.g <- graph.asn(ips,av.df,add.peers=TRUE,show.labels=FALSE)
asn.g <- graph.asn(ips,av.df,add.peers=FALSE,show.labels=FALSE)
plot(asn.g,vertex.label=NA,mark.groups=TRUE)

plot(walktrap.community(asn.g),asn.g,vertex.label=NA)

plot(cc.g,mark.groups=membership(walktrap.community(cc.g)))
plot(cc.g)

c <- walktrap.community(cc.g, steps=10)
v <- evcent(g)$vector
