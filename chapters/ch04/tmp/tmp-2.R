# smaller helper function to [l|r]trim blanks
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

BulkOrigin <- function(ip.list,host="v4.whois.cymru.com",port=43) {
  
  # setup query to bulk whois server
  cmd <- "begin\nverbose\n" 
  ips <- paste(unlist(ip.list), collapse="\n")
  cmd <- sprintf("%s%s\nend\n",cmd,ips)
  
  # creat the connection and post the query 
  con <- socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response <- readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response <- response[2:length(response)]
  # split and trim each response line
  response <- laply(response,.fun=function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })
  # put the results into a data frame
  response <- adply(response,c(1))
  # ignore the trailing field
  response <- subset(response, select = -c(X1) )
  # assign meaningful column names
  names(response) = c("AS","IP","BGP.Prefix","CC",
                      "Registry","Allocated","AS.Name")
  # return the result
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

# start graphing
g <- graph.empty()

# Make IP vertices; IP endpoints are red
g <- g + vertices(ips,size=2,color="red",group=1)

# Make BGP vertices
g <- g + vertices(unique(origin$AS),size=2,color="orange", group=2)

# Make IP/BGP edges
ip.edges <- lapply(ips,function(x) {
  iAS <- origin[origin$IP==x,]$AS
  lapply(iAS,function(y){
    c(x,y)
  })
})

# build ASN->IP edges
g <- g + edges(unlist(ip.edges))

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

# bar chart version
ggplot(dat=origin) + 
  geom_bar(aes(AS)) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

deg = degree(g)
deg.ordered = order(deg)

# get the ASNs info
# "attributes" get us the 'names'
a = attributes(deg[deg.ordered])

# prepend "AS" to the list
mal = unlist(lapply(grep("NA",a$names,value=TRUE,invert=TRUE),
                    function(x) sprintf("AS%s",x)))
mal.detail = BulkOriginASN(list(mal))

# get top 15 bad ASNs
mal.df = data.frame(deg[deg.ordered])
mal.df$AS = rownames(mal.df)
rownames(mal.df) = NULL
colnames(mal.df) = c("Count","AS")
