library(ggplot2)
# read the CSV with headers
za <- read.csv("data/zeroaccess.csv", header=T)

# create a scatter plot ggplot instance
gg <- ggplot(data=za, aes(x=long, y=lat)) 
# now just add the points, set transparency to 1/20th 
gg <- gg + geom_point(size=1, color="#000099", alpha=1/20) 
# add axes labels
gg <- gg + xlab("Longitude") + ylab("Latitude")
# simplify the theme to see points easier
gg <- gg + theme_bw() 
gg <- gg + coord_map("rectangular", 0)
print(gg)

# projections do ?mapproj


# now grab the "world" data
world <- map_data("world")
# nothing personal penguins, but strip out Antarctica
world <- subset(world, world$region!="Antarctica")
# load world data into ggplot object
gg <- ggplot(data=world, aes(x=long, y=lat))
# trace along the lat/long coords by group
gg <- gg + geom_path(aes(group=group), colour="gray70")
# now project using the mercator projection
gg <- gg + coord_map("mercator", xlim=c(-200, 200))
# load up the points by overiding the default data set
gg <- gg + geom_point(data=za, aes(long, lat), 
                      colour="#000099", alpha=1/40, size=1)
# add axes labels
gg <- gg + xlab("Longitude") + ylab("Latitude")
gg <- gg + theme_bw()
print(gg)

## let's look at generating a world chloropleth
# convert long/lat into country names
zworld <- latlong2map(data.frame(x=za$long, y=za$lat), "world")
# count up points in the country and conver to data frame
wct <- data.frame(table(zworld))
# label the country as "region" to match map data 
colnames(wct) <- c("region", "count")
# merge will match on "region" and add "count"
za.chloro <- merge(world, wct)
# now we sort, otherwise it's disasterous
za.chloro <- za.chloro[with(za.chloro, order(group, order)), ]
# and plot
gg <- ggplot(za.chloro, aes(x=long, y=lat, group=group, fill=count))
gg <- gg + geom_path(colour="#666666") + geom_polygon()
gg <- gg + coord_map("mercator", xlim=c(-200, 200), ylim=c(-60,200))
gg <- gg + scale_fill_gradient2(low="#FFFFFF", high="#4086AA", 
                                midpoint=median(za.chloro$count))
gg <- gg + theme_plain()
print(gg)

# get a vector of proportions
# for each wct$count, divide by sum
perc <- wct$count/sum(wct$count)
# covert to a readable format
wct$perc <- round(perc, 4)*100
# now order the highest proportions on top
wct <- wct[with(wct, order(perc, decreasing=T)), ]
# look at the top few entries.
head(wct)

zstate <- latlong2map(data.frame(x=za$long, y=za$lat), "state")
# select rows where the state is not NA
za.state <- za[which(!is.na(zstate)), ]

# load map data of the U.S.
state <- map_data("state")
gg <- ggplot(data=state, aes(x=long, y=lat))
gg <- gg + geom_path(aes(group=group), colour="gray80")
gg <- gg + coord_map("mercator")
gg <- gg + geom_point(data=za.state, aes(long, lat), 
                      colour="#000099", alpha=1/40, size=1)
gg <- gg + theme_plain()
print(gg)
#ggsave("793725c05f005.pdf", gg)

# create a chloropleth of the U.S. states
# because all of these vectors are from the same source (za), 
# we can cross indexes like this
# pull out those that are not NA, and not rounded off in Potwin
state.index <- which(!is.na(zstate) & za$lat!=38 & za$long!=-97)
# now create a count of states and filter on those indexes
sct <- data.frame(table(zstate[state.index]))
colnames(sct) <- c("region", "count")
# merge with state map data
za.sct <- merge(state, sct)
# Now plot a choropleth using a diverging color
colors <- suda.pal(5, "div")
gg <- ggplot(za.sct, aes(x=long, y=lat, group=group, fill=count))
gg <- gg + geom_polygon(colour="black")
gg <- gg + coord_map("polyconic")
gg <- gg + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=mean(za.sct$count))
gg <- gg + theme_plain()
print(gg)

# 34567890123456789012345678901234567890123456789012345678901234567890

# read in state population and internet users
# data scraped from http://www.internetworldstats.com/stats26.htm
users <- read.csv("data/state-internets.csv", header=T)
# all the state names are lower case in map data, so convert
users$state <- tolower(users$state)
# view the first few rows if you wish
head(users)
# output:
#         state population internet
# 1     alabama    4758191  3092273
# 2     arizona    6665093  5230474
# 3    arkansas    2919815  1949869
# 4  california   37350092 29758896
# 5    colorado    5077553  4058749
# 6 connecticut    3555261  3074229

# now merge with the sct data from previous example
# merge by sct$region and users$state
za.users <- merge(sct, users, by.x="region", by.y="state")
za.users$pop2inf <- round(za.users$population/za.users$count, 0)
za.norm <- data.frame(region=za.users$region,
                      count=za.users$pop2inf)
za.norm.map <- merge(state, za.norm)

gg <- ggplot(za.norm.map, aes(x=long, y=lat, group=group, fill=count))
gg <- gg + geom_polygon(colour="black")
gg <- gg + coord_map("polyconic")
gg <- gg + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=mean(za.norm.map$count))
gg <- gg + theme_plain()
print(gg)

# of internet users
za.users$pop2int <- round(za.users$internet/za.users$count, 0)
za.norm <- data.frame(region=za.users$region,
                      count=za.users$pop2int)
za.norm.map <- merge(state, za.norm)

gg <- ggplot(za.norm.map, aes(x=long, y=lat, group=group, fill=count))
gg <- gg + geom_polygon(colour="black")
gg <- gg + coord_map("polyconic")
gg <- gg + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=mean(za.norm.map$count))
gg <- gg + theme_plain()
print(gg)

# z-score 
za.sd <- sd(za.norm$count)
za.mean <- mean(za.norm$count)
za.norm$z <- round((za.norm$count-za.mean)/za.sd, 1)

za.users <- merge(sct, users, by.x="region", by.y="state")
za.users$pop2inf <- round(za.users$population/za.users$count, 0)
za.norm <- data.frame(region=za.users$region,
                      count=za.users$pop2inf)
za.norm.map <- merge(state, za.norm)

gg <- ggplot(za.norm.map, aes(x=long, y=lat, group=group, fill=z))
gg <- gg + geom_polygon(colour="black")
gg <- gg + coord_map("polyconic")
gg <- gg + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=0)
gg <- gg + theme_plain()
print(gg)

## now to county
county <- latlong2map(data.frame(x=za$long, y=za$lat), "county")
za.county <- county[which(!is.na(county) & za$lat!=38 & za$long!=-97)]
county.count <- table(za.county)
temp.list <- strsplit(names(county.count), ",")
za.county <- data.frame(matrix(unlist(temp.list), ncol=2, byrow=T), as.vector(county.count))
colnames(za.county) <- c("region", "subregion", "infections")


foo <- read.csv("/Users/jay/Documents/book/data/src/county-census.csv", header=T)
za.county2 <- merge(foo, za.county, all.x=T)

za.county2$pop2inf <- za.county2$infections/za.county2$pop
p2i.sd <- sd(za.county2$pop2inf)
p2i.mean <- mean(za.county2$pop2inf)
za.county2$z <- (za.county2$pop2inf - p2i.mean)/p2i.sd

za.county2$zlabel <- cut(trunc(za.county2$z), breaks=c(seq(-3, 3), 10),
             labels=c("-2 to -3", "-1 to -2", "0 to -1", "0 to 1", "1 to 2", "2 to 3", "outlier"))
cols <- c(rev(brewer.pal(5, "BrBG")), "#AA3333")

county <- map_data("county")
county <- merge(county, za.county2)
county <- county[with(county, order(group, order)), ]

county$zcopy <- ifelse(county$z>3,3,county$z)
gg <- ggplot(county, aes(x=long, y=lat, group=group, fill=z))
gg <- gg + geom_polygon(colour="#66AA660F", alpha=1, line=0)
gg <- gg + coord_map("polyconic")
gg <- gg + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=0)
gg <- gg + theme_plain()
print(gg)
gg <- gg + scale_fill_manual(values=cols)


