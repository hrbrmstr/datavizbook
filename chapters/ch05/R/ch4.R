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
print(gg)

# now grab the "world" data
world <- map_data("world")
# and strip out antarctica
world <- subset(world, world$region!="Antarctica")
# projections do ?mapproj
# mercator, sinusoidal, cylindrical, mollweide, gilbert
gg <- ggplot(data=world, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="#CCCCCC") + coord_map("mercator")
gg <- gg + scale_x_continuous(limits = c(-200, 200)) + # weird fix for linex across the map
gg <- gg + geom_point(data=za, aes(long, lat), colour="#00009902", size=1.5) + theme_plain()
print(gg)