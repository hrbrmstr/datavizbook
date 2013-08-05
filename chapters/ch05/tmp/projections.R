library(gridExtra)
library(ggplot2)

theme_plain <- function() {
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid =  element_line(colour = "gray70", size=1),
        axis.ticks.length = unit(0, "cm"),
        axis.ticks.margin = unit(0, "cm"),
        panel.margin = unit(0, "lines"),
        plot.margin = unit(c(0,0,0,0), "lines"),
        complete=TRUE)
}

theme_map <- function() {
  theme(
    line = element_line(colour="black"),
    axis.title = element_blank(),
    axis.text = element_blank(),
#    panel.background = element_blank(),
    panel.grid =  element_line(colour="black", size=1),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
#    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0,0,0,0), "lines")
#    complete=TRUE
    )
}

# mercator 
# polyconic
# rectangualr
# winkel tripel

world <- map_data("world")
world <- subset(world, world$region!="Antarctica")
world <- subset(world, world$region!="Greenland")
gg <- ggplot(data=world, aes(x=long, y=lat, group=group))
gg <- gg + geom_polygon(colour="gray50", fill="gray95")
gg <- gg + theme_boo()
gg2 <- gg + coord_map("mercator", orientation=c(90, 0, 0))
print(gg2)
#gg <- gg + coord_map("mercator")
gg <- ggplot(data=world, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_boo() + xlab("") + ylab("")
gg2 <- gg + coord_map("rectangular", lat0 = 30, orientation=c(90, 0, 0))
print(gg2)




theme_boo <- function(base_size = 12, base_family = "") {
  # Starts with theme_grey and then modify some parts
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      legend.key        = element_rect(colour = "grey80"),
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "grey60"),
      panel.grid.major  = element_line(colour = "grey90", size = 0.5),
      panel.grid.minor  = element_line(colour = "grey90", size = 0.5),
      strip.background  = element_rect(fill = "grey80", colour = "grey50"),
      strip.background  = element_rect(fill = "grey80", colour = "grey50")
    )
}

world <- map_data("world")
states <- map_data("state")
show <- FALSE

#Mercator
gg <- ggplot(data=world, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_boo() + xlab("") + ylab("") + ggtitle("Mercator")
gg <- gg + coord_map("mercator", orientation=c(90, 0, 0))
if(show) print(gg)
mer1 <- gg
gg <- ggplot(data=states, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_plain() + xlab("") + ylab("") 
gg <- gg + coord_map("mercator", orientation=c(90, 0, 0))
if(show) print(gg)
mer2 <- gg

#Polyconic
gg <- ggplot(data=world, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_boo() + xlab("") + ylab("") + ggtitle("Polyconic")
gg <- gg + coord_map("polyconic", orientation=c(90, 0, 0))
if(show) print(gg)
poly1 <- gg
gg <- ggplot(data=states, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_plain() + xlab("") + ylab("") 
gg <- gg + coord_map("polyconic")
if(show) print(gg)
poly2 <- gg

#Polyconic
gg <- ggplot(data=world, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_boo() + xlab("") + ylab("") + ggtitle("Polyconic")
gg <- gg + coord_map("polyconic", orientation=c(90, 0, 0))
if(show) print(gg)
poly1 <- gg
gg <- ggplot(data=states, aes(x=long, y=lat, group=group))
gg <- gg + geom_path(colour="gray50", fill="gray95")
gg <- gg + theme_plain() + xlab("") + ylab("") 
gg <- gg + coord_map("polyconic")
if(show) print(gg)
poly2 <- gg


grid.arrange(x1y1,x2y1,x1y2,x2y2, ncol=2, clip=T)



# the following is from:
# http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot/

library(rgdal)
library(ggplot2)

# read shapefile
wmap <- readOGR(dsn="data/map/ne_110m_land", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)

# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#ffffff"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

# reproject from longlat to robinson
wmap_robin <- spTransform(wmap, CRS("+proj=poly"))
wmap_df_robin <- fortify(wmap_robin)
ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (robinson)") + 
  coord_equal() +
  theme_opts

grat <- readOGR("data/map/ne_110m_graticules_all", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)

bbox <- readOGR("data/map/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)

ggplot(bbox_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map + graticule (longlat)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)

ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=wmap_df_robin, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

# add country borders
countries <- readOGR("data/map/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 
countries_robin <- spTransform(countries, CRS("+init=ESRI:54030"))
countries_robin_df <- fortify(countries_robin)

ggplot(bbox_robin_df, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries_robin_df, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  geom_path(data=grat_df_robin, aes(long, lat, group=group, fill=NULL), linetype="dashed", color="grey50") +
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

# Winkel tripel projection
countries_wintri <- spTransform(countries, CRS("+proj=wintri"))
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))
wink <- ggplot(bbox_wintri, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=grat_wintri, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey80") +
  geom_polygon(data=countries_wintri, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries_wintri, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="Winkel Tripel") + 
  coord_equal(ratio=1) + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

# poly
countries_poly <- spTransform(countries, CRS("+proj=poly"))
bbox_poly <- spTransform(bbox, CRS("+proj=poly"))
#wmap_wintri <- spTransform(wmap, CRS("+proj=poly"))
grat_poly <- spTransform(grat, CRS("+proj=poly"))
poly <- ggplot(bbox_poly, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=grat_poly, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey80") +
  geom_polygon(data=countries_poly, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries_poly, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="Polyconic") + 
  coord_equal(ratio=1) + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

# merc
countries_merc <- spTransform(countries, CRS("+proj=merc"))
bbox_poly <- spTransform(bbox, CRS("+proj=poly"))
#wmap_wintri <- spTransform(wmap, CRS("+proj=poly"))
grat_poly <- spTransform(grat, CRS("+proj=poly"))
ggplot(bbox_poly, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=grat_poly, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey80") +
  geom_polygon(data=countries_poly, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries_poly, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="Polyconic Projection") + 
  coord_equal(ratio=1) + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

# dunno, prolly equirectangular
equirect <- ggplot(bbox, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=grat, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey80") +
  geom_polygon(data=countries, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="Equirectangular") + 
  coord_equal(ratio=1) + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

grid.arrange(equirect, poly, wink, ncol=3, clip=T)
