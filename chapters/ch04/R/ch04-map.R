setwd("~/Dropbox/datavizbook/chapters")
avRep <- "ch03/data/reputation.data"
av.df <- read.csv(avRep,sep="#",header=FALSE)
colnames(av.df) <- c("IP","Reliability","Risk","Type",
                  "Country","Locale","Coords","x")

av.coords.df <- as.data.frame(
  matrix(unlist(strsplit(as.character(av.df$Coords),",")), 
         nrow=nrow(av.df), byrow=TRUE),stringsAsFactors=FALSE)
colnames(av.coords.df) = c("lat","long")
av.coords.df$long = as.numeric(av.coords.df$long)
av.coords.df$lat = as.numeric(av.coords.df$lat)

theme_clean <- function(base_size = 12) { require(grid) # Needed for unit() function
                                          theme_grey(base_size) %+replace%
                                            theme(
                                              axis.title = element_blank(),
                                              axis.text = element_blank(), panel.background = element_blank(),
                                              axis.ticks.length = unit(0, "cm"), axis.ticks.margin = unit(0, "cm"),
                                              panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE
                                            ) }


# install.packages(c("maps","mapproj","grid"))

library(maps)
library(mapproj)

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
gg <- gg + geom_point(data=av.coords.df, aes(long, lat), 
                      colour="#000099", alpha=1/40, size=1)
# add axes labels
gg <- gg + xlab("Longitude") + ylab("Latitude")
gg <- gg + theme_clean()
print(gg)




