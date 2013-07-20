library(ggplot2)
library(maps)
library(colorspace)
zero <- read.csv("/home/jay/mac/zerogeo.csv", header=T)
states <- map_data("state")
zero.clean <- data.frame(region=tolower(zero$state), 
                         perBot=round(zero$intUsers/zero$bots),
                         intUsers=zero$intUsers)
choro <- merge(states, zero.clean, sort = FALSE, by = "region")

choro <- choro[order(choro$order),]

choro$botBreaks <- cut(choro$perBot, 10)
choro$intBreaks <- cut(choro$intUsers,10)
# let's relabel the break points so they're pretty
labs <- levels(choro$botBreaks)
lower <- as.numeric( sub("\\((.+),.*", "\\1", labs) )
upper <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) )

c1 <- qplot(long, lat, data = choro, group = group, fill = botBreaks, geom = "polygon", 
           main="Population of Internet Users to One Zero Access Botnet Infenction") +
             theme(axis.line=element_blank(),axis.text.x=element_blank(),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),plot.background=element_blank())
c1 + scale_fill_manual(values=rev(brewer.pal(n=length(lower), "PuRd")),
                       labels=lower, name="Users per Bot")
# notice we reversed the colors, darker means "bad" (less people per bot)

# Population of internet users:
if(0) {
  c2 = qplot(long, lat, data = choro, group = group, fill = intBreaks, geom = "polygon", 
             main="Population of Internet Users") +
               theme(axis.line=element_blank(),axis.text.x=element_blank(),
                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),plot.background=element_blank())
  c2 + scale_fill_brewer(palette = "BrBG")
  
}
