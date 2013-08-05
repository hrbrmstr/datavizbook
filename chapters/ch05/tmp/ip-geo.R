ips <- read.csv("/Users/jay/Documents/book/data/src/city_blocks.csv", header=T)
ips$count <- ips$ip_end-ips$ip_start
ips <- aggregate(count ~ loc_id, data=ips, FUN=sum)

locs <- read.csv("/Users/jay/Documents/book/data/src/US_city_location.csv", header=T)

locs <- locs[ ,c(1,6,7)]

all.ips <- merge(locs, ips)
all.ips <- all.ips[which(all.ips$latitude!=38 & all.ips$longitude!=-97), ]
all.ips$county <- latlong2map(data.frame(x=all.ips$longitude, y=all.ips$latitude), "county")
delist <- function(x) {
  if(is.na(x[1])) {
    c(NA, NA)
  } else {
    c(x[1], x[2])
  }
}
foo <- unlist(lapply(strsplit(all.ips$county, ","), delist))
temp.list <- data.frame(matrix(foo, ncol=2, byrow=T))
all.ips$region <- temp.list$X1
all.ips$subregion <- temp.list$X2
save.ips <- all.ips[which(!is.na(all.ips$region)), c(4,6,7) ]

foo <- aggregate(count ~ region + subregion, data=save.ips, FUN=sum)
goo <- foo[with(foo, order(region,subregion)),]
write.csv(goo, "data/ipaddr-count.csv", row.names=F)

garbage <- merge(county.census, goo, all=T)
#garbage[is.na(garbage$count), c(1,2,3)]
garbage$count[is.na(garbage$count)] <- 0

write.csv(garbage, "data/census-ip.csv", row.names=F)
