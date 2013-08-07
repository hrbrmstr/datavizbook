#
# CODE FOR CHAPTER 4
#
options(width=70)

#
# Listing 4-1
#
library(bitops) # load the bitops functions
# take an IP address string in dotted octets (e.g. "192.168.0.1")
# and convert it to a 32-bit long integer (e.g. 3232235521)
ip2long <- function(ip) {
  # convert string into vector of characters
  ips <- unlist(strsplit(ip, '.', fixed=TRUE))
  # set up a function to bit-shift, then "OR" the octets
  arity <- function(x,y) bitOr(bitShiftL(x, 8), y)
  # Reduce applys a funcution cumulatively left to right
  Reduce(arity, as.integer(ips))
}

# take an 32-bit integer IP address (e.g. 3232235521)
# and convert it to a (e.g. "192.168.0.1").
long2ip <- function(longip) {
  # set up reversing bit manipulation
  arity <- function(nbits) bitAnd(bitShiftR(longip, nbits), 0xFF)
  # Map applys a function to each element of the arguent
  # paste converts arguments to character andconcatenates them
  paste(Map(arity, c(24,16,8,0)), sep="", collapse=".")
}

long2ip(ip2long("192.168.0.0"))
long2ip(ip2long("192.168.100.6"))

#
# Listing 4-1
#

# take an IP address (string) and a CIDR (string) and
# return whether the given IP address is in the CIDR range
ip.is.in.cidr <- function(ip, cidr) {
  long.ip <- ip2long(ip)
  cidr.parts <- unlist(strsplit(cidr, "/"))
  cidr.range <- ip2long(cidr.parts[1])
  cidr.mask <- bitShiftL(bitFlip(0), (32-as.integer(cidr.parts[2])))
  return(bitAnd(long.ip, cidr.mask) == bitAnd(cidr.range, cidr.mask))
}

ip.is.in.cidr("10.0.1.15","10.0.1.3/24")
ip.is.in.cidr("10.0.1.15","10.0.2.255/24")

#
# Listing 4-3: R code to extract longitude/latitude pairs from AlienVault data
#
# read in the AlienVault reputation data (see Chapter 3)
avRep <- "/suda/chapters/ch03/data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP", "Reliability", "Risk", "Type",
                     "Country", "Locale", "Coords", "x")

# create a vector of lat/long data by splitting on ","
av.coords.vec <- unlist(strsplit(as.character(av.df$Coords), ","))
# conver the vector in a 2-column matrix
av.coords.mat <- matrix(av.coords.vec, ncol=2, byrow=TRUE)
# project into a data frame
av.coords.df <- as.data.frame(av.coords.mat)
# name the columns 
colnames(av.coords.df) <- c("lat","long")
# convert the characters to numeric values
av.coords.df$long <- as.double(as.character(av.coords.df$long))
av.coords.df$lat <- as.double(as.character(av.coords.df$lat))

#
# Listing 4-4: R code to incorporate IANA IPv4 Allocations
#
# retrieve IANA prefix list
ianaURL <- "http://www.iana.org/assignments/ipv4-address-space/ipv4-address-space.csv"
ianaData <- "/suda/chapters/ch04/data/ipv4-address-space.csv"
if (file.access(ianaData)) {
  download.file(ianaURL, ianaData) 
}

# read in the IANA table
iana <- read.csv(ianaData)

# clean up the iana prefix since it uses the old/BSD-
# number formatting (i.e. allows leading zeroes and
# we do not need to know the CIDR component.
iana$Prefix <- sub("^(00|0)", "", iana$Prefix, perl=TRUE)
iana$Prefix <- sub("/8$", "", iana$Prefix, perl=TRUE)

# re-read the existing AlienVault data
avRep <- "/suda/chapters/ch03/data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP", "Reliability", "Risk", "Type",
                     "Country", "Locale", "Coords", "x")

# extract just the prefix from the AlienVault list
av.df$IP.prefix <- sapply(strsplit(as.character(av.df$IP), '.',
                                   fixed=TRUE), "[", 1)
# merge in iana data (faster than 'apply' operations)
av.df = merge(av.df,iana,by.x=c("IP.prefix"),by.y=c("Prefix"),all.x)
# summarize, order & review the findings
summary(factor(av.df$Designation))

#
# Listing 4-5: R code to extract IANA block assignments & compare with AlienVault groupings
#
# create a new data frame from the iana designation factors
iana.df <- data.frame(table(iana$Designation))
colnames(iana.df) <- c("Registry", "IANA.Block.Count")

# make a data frame of the counts of the av iana
# designation factor
tmp.df <- data.frame(table(factor(av.df$Designation)))
colnames(tmp.df) <- c("Registry", "AlienVault.IANA.Count")

# merge (join) the data frames on the "reg" column
combined.df <- merge(iana.df, tmp.df)
print(combined.df[with(combined.df, order(-IANA.Block.Count)),],
      row.names=FALSE)

#
# Listing 4-6: R code to plot IANA Charts
#
# flatten the data frame by making one entry per “count” type
# versus having the counts in individual columns
library(reshape)
library(ggplot2)
melted.df <- melt(combined.df)
# plot the new melted data frame values
gg <- ggplot(data=melted.df, aes(x=Registry, 
                                 y=value))
gg <- gg + geom_bar(aes(fill=variable), stat="identity") # using bars
# and creating two charts, side-by-side based on the two different
# count types; note we’ve changed the default behavior of facet_wrap()
# by letting it auto-adjust the y-axis scale. If we hadn’t, it would 
# have resulted in ggplot applying the larger of the two scales and 
# making the base IANA plot look almost blank.
gg <- gg + facet_wrap(~variable, scales="free_y") 
# make a better label for the y axis
gg <- gg + labs(y="Count") 
# rotate the x-axis labels and remove the legend
gg + theme(axis.text.x = element_text(angle = 90, hjust = 1), 
              legend.position = "none")

print(gg)

# 
# CODE FOR FIGURE 4.4
#
mfrow <- par()$mfrow
mar <- par()$mar
oma <- par()$oma

par(mfrow=c(2, 3), mar=c(0, 0, 2, 0), oma=c(1, 1, 1, 1))
set.seed(2)
x <- runif(500, min=0, max=pi)

y <- rnorm(500, mean=x, sd=0.2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=-x, sd=0.3)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=sin(x), sd=0.2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=x, sd=2)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- rnorm(500, mean=-x, sd=1)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

y <- runif(500, min=0, max=pi)
plot(x,y, pch=19, cex=.8, col="#666699CC", axes=FALSE, xlab="", ylab="", 
     main=paste("Correlation: ", round(cor(x,y), 2)))

par(mfrow=mfrow, mar=mar, oma=oma)

#
# BACK TO THE BOOK CODE
#

# this generates Figure 4.5
ggplot(data=combined.df) + 
  geom_point(aes(x=IANA.Block.Count, y=AlienVault.IANA.Count))

cor(combined.df$IANA.Block.Count,
    combined.df$AlienVault.IANA.Count, method="spearman")

#
# Listing 4-7: R code to read in the ZeuS blocklist
#
# retrieve ZeuS blocklist
zeusURL <- "https://zeustracker.abuse.ch/blocklist.php?download=ipblocklist"
zeusData <- "/suda/chapters/ch04/data/zeus.csv"
if (file.access(zeusData)) {
  # need to change download method for universal "https" compatibility
  download.file(zeusURL, zeusData, method="curl") 
}
# read in the ZeuS table; skip junk; no header; assign colnames
zeus <- read.table(zeusData, skip=5, header=FALSE, col.names=c("IP"))

# 
# HELPER FUNCTIONS MENTIONEDIN CHAPTER 4
#

library(igraph)
library(plyr)
library(colorspace)

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


#
# Listing 4-8: Building ZeuS blocklist in a graph structure by country
#
library(igraph)
ips <- as.character(zeus$IP) 
# get BGP origin data & peer data; 
origin <- BulkOrigin(ips)
g <- graph.empty() # start graphing
# Make IP vertices; IP endpoints are red
g <- g + vertices(ips, size=2, color="red", group=1)
# Make BGP vertices
g <- g + vertices(origin$CC, size=2, color="orange", group=2)
# for each IP address, get the origin AS CC and return 
# them as a pair to create the IP->CC edge list
ip.cc.edges <- lapply(ips, function(x) {
  iCC <- origin[origin$IP==x, ]$CC
  lapply(iCC, function(y){
    c(x, y)
  })
})
g <- g + edges(unlist(ip.cc.edges)) # build CC->IP edges
# simplify the graph by combining commmon edges
g <- simplify(g, edge.attr.comb=list(weight="sum"))
# delete any standalone vertices (lone wolf ASNs). In "graph" terms
# delete any vertex with a degree of 0
g <- delete.vertices(g, which(degree(g) < 1))
E(g)$arrow.size <- 0 # we hates arrows
# blank out all the IP addresses to focus on ASNs 
V(g)[grep("\\.", V(g)$name)]$name <- ""

# 
# Listing 4-9: Visualizing the ZeuS blocklist country cluster graph
#
# this is a great layout for moderately sized networks. you can
# tweak the "n=10000" if this runs too slowly for you. The more
# iterations, the cleaner the graph will look
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
# plot the graph
par(bg = 'white', mfrow=c(1,1))
plot(g, margin=0, layout=L, vertex.label.dist=0.5, 
     vertex.label.cex=0.75, 
     vertex.label.color="black",
     vertex.label.family="sans", 
     vertex.label.font=2,
     main="ZeuS botnet nodes clustered by country")

#
# Listing 4-10: Translate country codes to name
#
# read in country code to name translation table
zeus.cc <- grep("[A-Z]", V(g)$name, value=TRUE)
zeus.cc <- zeus.cc[order(zeus.cc)]
# read in the country codes data frame
cc.df <- read.csv("/suda/chapters/ch04/data/countrycode_data.csv")
# display cc & name for just the ones from our data set
print(head(cc.df[cc.df$iso2c %in% zeus.cc, c(7,1)], n=10),
      row.names=FALSE)

#
# Listing 4-10: Country code name translation
#
sort(table(factor(origin$CC)))


#
# Listing 4-11: Connected network of ZeuS IPs, ASNs & ASN Peers
#
g <- graph.empty()
g <- g + vertices(ips, size=2, color="red", group=1)
origin <- BulkOrigin(ips)
peers <- BulkPeer(ips)
# add ASN origin & peer vertices
g <- g + vertices(unique(c(peers$Peer.AS, origin$AS)),
                  size=2, color="orange", group=2)
# build IP->BGP edge list
ip.edges <- lapply(ips, function(x) {
  iAS <- origin[origin$IP==x, ]$AS
  lapply(iAS,function(y){
    c(x, y)
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
V(g)[grep("\\.", V(g)$name)]$name = ""
L <- layout.fruchterman.reingold(g, niter=10000, area=30*vcount(g)^2)
par(bg = 'white')
plot(g, margin=0, layout=L, vertex.label.dist=0.5, 
     vertex.label=NA,
     main="ZeuS botnet ASN+Peer Network")


#
# MORE HELPER FUNCTIONS
#

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
    unique(origin$CC),
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
  
  # add BGP->IP vertices and - if requested - add peer ASN vertices
  if (add.peers) { 
    g <- g + vertices(
      unique(c(peers$Peer.AS, origin$AS)),
      size=2,color="orange",group=2)
  } else {
    g <- g + vertices(
      unique(origin$AS),
      size=2,color="orange", group=2)
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
  L <- layout.fruchterman.reingold(g, niter=10000, 
                                   area=30*vcount(g)^2)
  
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


#
# Listing 4-12: Working With Real Data
#
avRep <- "/suda/chapters/ch03/data/reputation.data"
av.df <- read.csv(avRep, sep="#", header=FALSE)
colnames(av.df) <- c("IP", "Reliability", "Risk", "Type",
                     "Country", "Locale", "Coords", "x")
# read in list of destination IP addresses siphoned from firewall logs
dest.ips <- read.csv("/suda/chapters/ch04/data/dest.ips", 
                     col.names= c("IP"))
# take a look at the reliability of the IP address entries
# you could also plot a histogram
table(av.df[av.df$IP %in% dest.ips$IP, ]$Reliability)
# extract only the "bad" ones, designated by presence in alienvault
# database with a reliability greater than 6 since there seems to 
# be a trailing off at that point
ips <- as.character(av.df[(av.df$IP %in% dest.ips$IP) & 
                            (av.df$Reliability > 6), ]$IP)
# graph it
g.cc <- graph.cc(ips, av.df)



