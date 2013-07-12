library(ggplot2)
library(ggmap)
library(maps)
library(maptools)

za <- read.csv("ZeroAccessGeoIPs.csv", header=F, stringsAsFactors=F)
# splits out the second variable into a list of vectors
za <- lapply(strsplit(za$V2, ","), as.numeric)

# unlists the vectors and casts them into a data.frame
za <- data.frame(matrix(unlist(za), ncol=2, byrow=T))
# names the columns x and y
colnames(za) <- c("lat", "long")
#za$group <- 1

theme_plain <- function() {
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        axis.ticks.margin = unit(0, "cm"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0), "lines"),
        complete=TRUE)
}

# create just a simple scatter plot with bw theme
ggplot(data=za, aes(x=long, y=lat)) + geom_point(size=1.5, color="#000099", alpha=1/10) + theme_bw()

# now grab the "world" data
world <- map_data("world")
# and strip out antarctica
world <- world[-which(world$region=="Antarctica"), ]
# projections do ?mapproj
# mercator, sinusoidal, cylindrical, mollweide, gilbert
ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group), colour="#CCCCCC") + coord_map("mercator") +
  scale_x_continuous(limits = c(-200, 200)) + # weird fix for linex across the map
  geom_point(data=za, aes(long, lat), colour="#00009902", size=1.5) + theme_plain()
 
# testing lines across top
#world <- data.frame(map("world", plot=FALSE)[c("x","y")])
#ggplot( world, aes(x=x,y=y)) + geom_path( ) +coord_map() + scale_x_continuous(limits=(c(-200,200)))
# now just the state data
state <- map_data("state")
# want a function to just pull US points
inbetween <- function(data, arange) {
  which(data>=arange[1] & data<=arange[2])
}
za.state <- za[inbetween(za$lat, range(state$lat)), ]
za.state <- za.state[inbetween(za.state$long, range(state$long)), ]
# there is the potwin,KS problem:
za.state <- za.state[-which(za.state$lat==38 & za.state$long==-97), ]
ggplot() + geom_path(data=state, aes(x=long, y=lat, group=group), colour="#CCCCCC") + coord_map("mercator") +
  geom_point(data=za.state, aes(long, lat), colour="#000099", alpha=1/10, size=1) + theme_plain()

zstate <- latlong2state(data.frame(x=za.state$long, y=za.state$lat))
za.state <- cbind(za.state, data.frame(state=zstate))
colour <- ifelse(is.na(za.state$state), "red", "blue")
za.state <- cbind(za.state, data.frame(col=colour))
ggplot() + geom_path(data=state, aes(x=long, y=lat, group=group), colour="#CCCCCC") + coord_map("mercator") +
  geom_point(data=za.state, aes(long, lat, colour=col), alpha=1/8, size=2) + theme_plain()

# now just take out the points
za.state <- za.state[-which(is.na(za.state$state)), ]
ggplot() + geom_path(data=state, aes(x=long, y=lat, group=group), colour="#CCCCCC") + coord_map("mercator") +
  geom_point(data=za.state, aes(long, lat), colour="#000099", alpha=1/10, size=1) + theme_plain()

# now condense for a choropleth:
state.count <- data.frame(table(za.state$state))
colnames(state.count) <- c("region", "count")
za2 <- merge(state, state.count)

# not helpful.
ggplot(za2, aes(x=long, y=lat, group=group, fill=count)) + geom_polygon(colour="black") +
  coord_map("polyconic") + theme_plain()

# lets try "above average" and "below average"
za2$average <- ifelse(za2$count > mean(state.count$count), "Above", "Below")
ggplot(za2, aes(x=long, y=lat, group=group, fill=average)) + geom_polygon(colour="black") +
  coord_map("polyconic") + theme_plain()

# taken from winston
#ggplot(za2, aes(map_id = state, fill=count)) + geom_map(map=states, colour="black") + 
#  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=median(za2$count)) +
#  expand_limits(x=states$long, y=states_map$lat) + coord_map("polyconic")

# this is a good point to talk "base rate fallacy"

# copy and pasted table from http://www.internetworldstats.com/stats26.htm
users <- read.csv("state-internets.csv", header=T)
users$state <- tolower(users$state)
za3 <- merge(za2, users, by.x="region", by.y="state")

# look as a proportion of population:
za3$ofpop <- za3$count/za3$population
temp.za <- aggregate(ofpop ~ region, data=za3, FUN=mean )
za3$pop.average <- ifelse(za3$ofpop>mean(temp.za$ofpop), "Above", "Below")
ggplot(za3, aes(x=long, y=lat, group=group, fill=pop.average)) + geom_polygon(colour="black") +
  coord_map("polyconic") + theme_plain()

# still not quite right, how about as a proportion of internet users
za3$ofinternet <- za3$count/za3$internet
temp.za <- aggregate(ofinternet ~ region, data=za3, FUN=mean )
za3$int.average <- ifelse(za3$ofinternet>mean(temp.za$ofinternet), "Above", "Below")
ggplot(za3, aes(x=long, y=lat, group=group, fill=int.average)) + geom_polygon(colour="black") +
  coord_map("polyconic") + theme_plain()

# how about as a continuous (diverging) color?
ggplot(za3, aes(x=long, y=lat, group=group, fill=ofinternet)) + geom_polygon(colour="black") +
  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", midpoint=mean(za3$ofinternet)) +
  coord_map("polyconic") + theme_plain()

ggplot(za3, aes(x=long,  y=lat, group=group, fill=internet)) + geom_polygon(colour="black") +
  scale_fill_gradient2(low="#018571", mid="grey90", high="#BB650B", midpoint=median(users$internet)) +
  coord_map("stereographic") + theme_plain()

# may want to split into 5 groups or something for more fun
# okay, let's run to county level


# code below is also a gist on github somewhere
# https://gist.github.com/rweald/4720788

## This code taken from http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)

  stateNames[indices]
}

mksimple <- function(x) {
  if (!is.na(x)) {
    ret <- data.frame(long=c(NA), lat=c(NA))
  } else {
    splitvec <- unlist(strsplit(x, ','))
    ret <- data.frame(region=unlist)
  }
}

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}
zero <- read.csv("/home/jay/mac/zerogeo.csv", header=T)
states <- map_data("state")


clients <- read.csv("~/mac/book/data/clients.csv", header=T)
client.names <- clients$client
clients <- subset(clients, select=-c(client))
clients <- sapply(clients, function(x) ifelse(x>0, 1, 0))
foo <- rowSum(clients)
saved <- data.frame(count=foo[which(foo>3)], host=client.names[which(foo>3)])
head(allsave[with(allsave, order(-count)), ])

clients <- read.csv("~/mac/book/data/geoclient.csv", header=F)
za <- data.frame(lat=clients$V8, long=clients$V9)
clients <- clients[-which(is.na(clients$V8)), ]