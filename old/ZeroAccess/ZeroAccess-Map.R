library(ggplot2)
library(maps)
zero <- read.csv("/home/jay/mac/zerogeo.csv", header=T)
states <- map_data("state")
zero.clean <- data.frame(region=tolower(zero$state), 
                         perBot=round(zero$intUsers/zero$bots),
                         intUsers=zero$intUsers)
choro <- merge(states, zero.clean, sort = FALSE, by = "region")

choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, fill = perBot, geom = "polygon", 
      main="Population of Internet Users to One Zero Access Botnet Infenction") +
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
# Population of internet users:
if(0) {
  qplot(long, lat, data = choro, group = group, fill = intUsers, geom = "polygon", 
        main="Population of Internet Users") +
          theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())
  
}
