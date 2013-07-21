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
# use the zstate we created in the last step and remove NAs
sct <- data.frame(table(zstate, useNA="no"))
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
                                