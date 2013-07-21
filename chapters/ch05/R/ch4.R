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

# 34567890123456789012345678901234567890123456789012345678901234567890

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