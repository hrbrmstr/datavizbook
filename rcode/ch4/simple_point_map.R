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
ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group), colour="#CCCCCC") + coord_map("gilbert") +
  geom_point(data=za, aes(long, lat), colour="#00009902", size=1.5) + theme_plain()
  
# now just the state data
state <- map_data("state")
# want a function to just pull US points
inbetween <- function(data, arange) {
  which(data>=arange[1] & data<=arange[2])
}
za.state <- za[inbetween(za$lat, range(state$lat)), ]
za.state <- za.state[inbetween(za.state$long, range(state$long)), ]
ggplot() + geom_path(data=state, aes(x=long, y=lat, group=group), colour="#CCCCCC") + coord_map("mercator") +
  geom_point(data=za.state, aes(long, lat), colour="#000099", alpha=1/10, size=1) + theme_plain()

# ggplot(za, aes(y, x)) + geom_point(colour="#0000FF11")
# ggplot() + geom_path(data=world, aes(x=long, y=lat, group=group)) + geom_point(data=za, aes(y,x))
# ggplot(world, aes(x=long, y=lat, group=group)) + geom_path() + coord_map("mercator") + 
#   geom_point(data=za, aes(x=y, y=x))
# 
# p <- ggplot()
# p <- p + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white" )
# p <- p + geom_point( data=mydata, aes(x=long, y=lat, size = enrollment), color="coral1") + scale_size(name="Total enrollment")
# p <- p + geom_text( data=mydata, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=label), colour="gold2", size=4 )
# p

# code below is also a gist on github somewhere

## This code taken from http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

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