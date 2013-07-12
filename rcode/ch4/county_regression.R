book/data/src

all <- read.csv("~/mac/book/data/src/city_blocks.csv", header=T)
# we are at 2,020,167 rows
all$count <- all$ip_end-all$ip_start
all <- aggregate(count ~ loc_id, data=all, FUN=sum)
# now we are at 108088 rows


uscity <- read.csv("~/mac/book/data/src/US_city_location.csv", header=T)
full <- merge(uscity, all, by="loc_id", all.x=T)
full$count[is.na(full$count)] <- 0

usregion <- read.csv("~/mac/book/data/src/US_region_names.csv", header=T)
full <- merge(full, usregion, all.x=T)
state <- aggregate(count ~ region_code + region_name, data=full, FUN=sum)

users <- read.csv("~/mac/book/data/src/state-internets.csv", header=T)
allstates <- merge(state, users)
foo <- lm(allstates$count ~ allstates$population + allstates$internet)
summary(foo)


latlong <- unique(data.frame(long=full$longitude, lat=full$latitude))latlong <- unique(data.frame(long=full$longitude, lat=full$latitude))
latlong <- unique(data.frame(long=full$longitude, lat=full$latitude))
llong <- latlong2county(latlong)
latlong$county <- llong
full2 <- merge(full, latlong, by.x=c("latitude", "longitude"), by.y=c("lat", "long"))

state2 <- aggregate(count ~ county + region_code + region_name, data=full2, FUN=sum)
state2$county <- sapply(strsplit(state2$county, ","), function(x) x[2])

census <- read.csv("~/mac/book/data/src/census.csv", header=T)
census$county <- tolower(census$county)
census$high <- census$high*.01*census$pop
census$college <- census$college*.01*census$pop

foo <- merge(state2, census, all=T)
full3 <- foo[-which(is.na(foo$count) | is.na(foo$pop)), ]

full3$count[full3$count==0] <- 1

ggplot(data = full3,aes(x = pop, y=count)) + 
  geom_point() +   scale_y_log10() + scale_x_log10() +
  stat_smooth(method = "lm", formula = y ~ x, size = 1)

ggpairs(full3, columns=4:9)
full4 <- data.frame(log_ip=log10(full3$count), log_pop=log10(full3$pop), 
                    high=log10(full3$high), college=log10(full3$college),
                    log_income=log10(full3$income), log_permile=log10(full3$permile))
ggpairs(full4)

goodlm <- lm(log(full3$count) ~ log(full3$pop) + log(full3$permile))
summary(goodlm)
# the populations with "0" are outliers here... weird.

# 1.565b total IPs in US
# 1.168b able to be tied to county
# 332m just tied to "US"
# 64m lost in county resolutions


full3$ratio <- full3$count/full3$pop
head(full3[with(full3, order(-ratio, count)), ])

#cochise, az
az <- full2[which(full2$region_code=="AZ"), ]
az <- full2[5677, ]
goo <- read.csv("~/mac/book/data/src/city_blocks.csv", header=T)
goo <- goo[which(goo$loc_id==5807), ]
goo$count <- goo$ip_end - goo$ip_start
head(goo[with(goo, order(-count)), ])
# then lookup ip 

oh <- full2[which(full2$region_code=="OH"), ]
oh <- full2[which(full2$county=="ohio,franklin"), ]
oh[with(oh, order(-count)), ]
ohio,franklin 8432

