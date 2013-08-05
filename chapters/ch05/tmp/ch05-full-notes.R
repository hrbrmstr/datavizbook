library(ggplot2)
library(scales)

########################################################
# read the CSV with headers
za <- read.csv("data/zeroaccess.csv", header=F)

# create a ggplot instance with zeroaccess data
gg <- ggplot(data=za, aes(x=long, y=lat)) 
# add the points, set transparency to 1/40th 
gg <- gg + geom_point(size=1, color="#000099", alpha=1/40) 
# add axes labels
gg <- gg + xlab("Longitude") + ylab("Latitude")
# simplify the theme for aesthetics
gg <- gg + theme_bw() 
print(gg)

########################################################

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
za.sct <- za.sct[with(za.sct, order(group, order)), ]

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

# create a box plot of the count
popbox <- boxplot(za.norm$count, 
                  main="Distribution of Normalized State Infections")
za.norm[za.norm$count %in% popbox$out, ]

# z-score 
# get the standard deviation
za.sd <- sd(za.norm$count)
# get the mean
za.mean <- mean(za.norm$count)
# now calculate the z-score and round to 1 decimal
za.norm$z <- round((za.norm$count-za.mean)/za.sd, 1)
# print a count of entries within each std dev
print(table(abs(trunc(za.norm$z))+1))
                         

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

set.seed(1492)
mean(runif(100, min=98, max=102))

#setting seed for reproducibility
set.seed(1492)
parts <- sapply(seq(10000), function(x) mean(runif(100, min=98, max=102)))
range(parts)

## now to county
county <- latlong2map(data.frame(x=za$long, y=za$lat), "county")
# system.time({ county <- latlong2map(data.frame(x=za$long, y=za$lat), "county") }); flush.console();
za.county <- county[which(!is.na(county) & za$lat!=38 & za$long!=-97)]
county.count <- table(za.county)
# need to convert "county, state" into a data frame
# so we split it out by comma
temp.list <- strsplit(names(county.count), ",")
# convert the list into a vector
temp.list <- unlist(temp.list)
# force the vector into a 2 column matrix, filling row by row
temp.matrix <- matrix(temp.list, ncol=2, byrow=T)
# and now create the data frame with the count of county infections
za.county <- data.frame(temp.matrix, as.vector(county.count))
# finally assign names to the fields
# names match the field names in the county map_data 
colnames(za.county) <- c("region", "subregion", "za")

cdata <- read.csv("data/county-data.csv", header=T) # with ipaddr
county.census <- read.csv("data/county-census.csv", header=T)
ufo.in <- read.csv("data/pop-inc-ufo.csv")
# notice the all.x here.
za.county <- merge(county.census, za.county, all.x=T)
za.county <- merge(ufo.in, za.county, all.x=T)
za.county <- merge(cdata, za.county, all.x=T)
# replace all NA's in ZeroAccess counts with 0
za.county$za[is.na(za.county$za)] <- 0

foo <- za.county[ , c(7,3,4,5,6)]
foo$za[is.na(foo$za)] <- 0
foo$za.log <- ifelse(foo$za==0, 0, log(foo$za))
foo$pop.log <- ifelse(foo$pop==0, 0, log(foo$pop))
foo$za.log <- log(foo$za)
foo$pop.log <- log(foo$pop)
ggpairs(foo)
foo <- lm(za ~ pop + income + ufo2pop, data=za.county)
foo <- lm(za ~ pop + income + ufo2010, data=za.county)

fit <- lm(za ~ )
sqrt(vif(fit)) > 2 # problem?


za.county2$pop2inf <- za.county2$infections/za.county2$pop
p2i.sd <- sd(za.county2$pop2inf)
p2i.mean <- mean(za.county2$pop2inf)
za.county2$z <- (za.county2$pop2inf - p2i.mean)/p2i.sd

za.county2$zlabel <- cut(trunc(za.county2$z), breaks=c(seq(-3, 3), 10),
             labels=c("-2 to -3", "-1 to -2", "0 to -1", "0 to 1", "1 to 2", "2 to 3", "outlier"))
cols <- c(rev(brewer.pal(5, "BrBG")), "#AA3333")

za.c <- subset(za.county, select=c(subregion, region, pop, za))
#county$zcopy <- ifelse(county$z>3,3,county$z)
za.county$logza <- ifelse(za.county$za==0, 0, log10(za.county$za))
za.county$logpop <- ifelse(za.county$pop==0, 0, log10(za.county$pop))
za.county$lufo <- ifelse(za.county$ufo2010==0, 0, log10(za.county$ufo2010))
county <- map_data("county")
county <- merge(county, za.county)
county <- county[with(county, order(group, order)), ]

# 34567890123456789012345678901234567890123456789012345678901234567890
gpop <- ggplot(county, aes(x=long, y=lat, group=group, fill=logpop))
gpop <- gpop + geom_polygon(colour="#66AA660F", alpha=1, line=0)
gpop <- gpop + coord_map("polyconic")
gpop <- gpop + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=mean(county$logpop),
                                        guide=F)
gpop <- gpop + ggtitle("U.S. Population")
gpop <- gpop + theme_plain()
#print(gg.pop)
ginf <- ggplot(county, aes(x=long, y=lat, group=group, fill=logza))
ginf <- ginf + geom_polygon(colour="#66AA660F", alpha=1, line=0)
ginf <- ginf + coord_map("polyconic")
ginf <- ginf + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                high=colors[1], 
                                midpoint=mean(county$logza),
                                        guide=F)
ginf <- ginf + ggtitle("ZeroAccess Infections")
ginf <- ginf + theme_plain()
#print(gg.inf)
grid.arrange(ginf,gpop)


gg.ufo <- ggplot(county, aes(x=long, y=lat, group=group, fill=lufo))
gg.ufo <- gg.ufo + geom_polygon(colour="#66AA660F", alpha=1, line=0)
gg.ufo <- gg.ufo + coord_map("polyconic")
gg.ufo <- gg.ufo + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                        high=colors[1], 
                                        midpoint=mean(county$lufo),
                                        guide=F)
gg.ufo <- gg.ufo + ggtitle("UFO sightings")
gg.ufo <- gg.ufo + theme_plain()
print(gg.ufo)

base.mod <- za.county$pop/1000
za.county$za.norm <- za.county$za/base.mod
za.county$ufo.norm <- za.county$ufo2010/base.mod
county <- merge(county, za.county)
county <- county[with(county, order(group, order)), ]
county$lza <- ifelse(county$za.norm==0, 0, log10(county$za.norm))

gg.ufo <- ggplot(county, aes(x=long, y=lat, group=group, fill=lza))
gg.ufo <- gg.ufo + geom_polygon(colour="#66AA660F", alpha=1, line=0)
gg.ufo <- gg.ufo + coord_map("polyconic")
gg.ufo <- gg.ufo + scale_fill_gradient2(low=colors[5], mid=colors[3], 
                                        high=colors[1], 
                                        midpoint=mean(county$lza),
                                        guide=F)
gg.ufo <- gg.ufo + ggtitle("ZeroAccess Infections per 1,000 people")
gg.ufo <- gg.ufo + theme_plain()
print(gg.ufo)



library(gridExtra)
grid.arrange(gg.inf, gg.pop, gg.ufo)
my.lm <- function(data, ...) {
  local.data <- data[sample(nrow(data), size=50), ]
  fit <- lm(data=local.data, ...)
  local.data <- data[sample(nrow(data), size=50), ]
  predict.lm(fit, newdata=local.data)
}
fit <- my.lm(infections ~ pop + college + income + poppermile, data=za.c)


# trying some simple scatterplots
library(scales)
gg <- ggplot(data=za.county, aes(x=pop, y=za)) + geom_point()
print(gg)

gg <- ggplot(data=za.county, aes(x=pop, y=za)) + geom_point()
gg <- gg + scale_y_continuous(trans=log10_trans())
gg <- gg + scale_x_continuous(trans=log10_trans())
print(gg)

gg <- ggplot(data=za.county, aes(x=pop, y=za)) + geom_point()
gg <- gg + scale_y_continuous(limits=c(0,300))
gg <- gg + scale_x_continuous(limits=c(0,250000))
print(gg)

gg <- ggplot(data=za.county, aes(x=income, y=za)) + geom_point()
gg <- gg + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
gg <- gg + scale_y_continuous(trans=log10_trans())
gg <- gg + scale_x_continuous(trans=log10_trans())
print(gg)

gg <- ggplot(data=za.county, aes(x=ufo.norm, y=za.norm)) + geom_point()
gg <- gg + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
gg <- gg + scale_y_continuous(trans=log10_trans())
gg <- gg + scale_x_continuous(trans=log10_trans())
print(gg)

library(car)
sqrt(vif(foo)) > 2 # problem?
foo <- lm(za ~ pop + income + ufo2pop, data=za.county)

pvals <- NULL
for(i in seq(1:1000)) {
  model <- lm(za ~ pop + income, data=za.county[sample(nrow(za.county), 100), ])
  model.summary <- summary(model)
  pvals <- c(pvals, model.summary$coefficients[3,4])
}
mean(pvals)
summary(pvals)
sum(pvals<0.05)

#print(model.summary$r.squared)
#print(model.summary$coefficients[,4])


#make a picture for linear regression

temp.za <- za.county[za.county$za<200, ]
doo <- temp.za[sample(nrow(temp.za), 40), ]; cor(doo$za, doo$pop)

ggplot(doo, aes(doo$income, doo$za)) + geom_point() + 
geom_smooth(method = "lm", se=F, color="red", formula = y ~ x)

foo <- lm(za ~ income, data=za.county)
goo <- summary(foo)
doo <- za.county[which(goo$residuals>-50 & goo$residuals<50), ]

ggplot(doo, aes(doo$income, doo$za)) + geom_point() + 
  geom_smooth(method = "lm", se=F, color="red")

# for reproducability
set.seed(1)
# generate 200 random numbers around 10
input <- rnorm(200, mean=10)
# generate output around a mean of 2 x input
output <- rnorm(200, mean=input*2)
# put into data frame to plot it
our.data <- data.frame(input, output)
# and plot it
gg <- ggplot(our.data, aes(input, output))
gg <- gg + geom_point()
gg <- gg + ggtitle("A Sample Linear Relationship")
gg <- gg + geom_smooth(method = "lm", se=F, color="red")
gg <- gg + theme_bw()
print(gg)

model <- lm(output ~ input, data=our.data)
summary(model)

summary(lm(za ~ ufo2010, data=za.county))
library(car)
sqrt(vif(foo)) > 2 # problem?
model <- lm(za ~ pop + income + ipaddr + ufo2010, data=za.county)
summary(lm(za ~ pop + income + ipaddr + ufo2010, data=za.county))

za.county$za.by.pop <- za.county$za/za.county$pop
za.county$ufo.by.pop <- za.county$ufo2010/za.county$pop
summary(lm(za.by.pop ~ ufo.by.pop, data=za.county))

summary(lm(za ~ pop, data=za.county))