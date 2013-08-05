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
                         plot.title = element_text(size=16)))

# reproject from longlat to robinson
wmap_robin <- spTransform(wmap, CRS("+proj=poly"))
wmap_df_robin <- fortify(wmap_robin)
grat <- readOGR("data/map/ne_110m_graticules_all", layer="ne_110m_graticules_15") 
grat_df <- fortify(grat)
bbox <- readOGR("data/map/ne_110m_graticules_all", layer="ne_110m_wgs84_bounding_box") 
bbox_df<- fortify(bbox)
grat_robin <- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df_robin <- fortify(grat_robin)
bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
bbox_robin_df <- fortify(bbox_robin)
countries <- readOGR("data/map/ne_110m_admin_0_countries", layer="ne_110m_admin_0_countries") 
countries_robin <- spTransform(countries, CRS("+init=ESRI:54030"))
countries_robin_df <- fortify(countries_robin)

# Winkel tripel projection
countries_wintri <- spTransform(countries, CRS("+proj=wintri"))
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))
wink <- ggplot(bbox_wintri, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=grat_wintri, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey80") +
  geom_polygon(data=countries_wintri, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries_wintri, aes(long,lat, group=group, fill=hole), color="grey60", size=0.2) +
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
  geom_path(data=countries_poly, aes(long,lat, group=group, fill=hole), color="grey60", size=0.2) +
  labs(title="Polyconic") + 
  coord_equal(ratio=1) + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

# dunno, prolly equirectangular
equirect <- ggplot(bbox, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_path(data=grat, aes(long, lat, group=group, fill=NULL), linetype="solid", color="grey80") +
  geom_polygon(data=countries, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries, aes(long,lat, group=group, fill=hole), color="grey60", size=0.2) +
  labs(title="Equirectangular") + 
  coord_equal(ratio=1) + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend

grid.arrange(equirect, poly, wink, ncol=3, clip=T)
grid.arrange(equirect, poly, wink, ncol=1, clip=T)
