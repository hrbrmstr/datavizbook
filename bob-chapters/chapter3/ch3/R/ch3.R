# 
# ch3.R
#
# sample analysis script for AlienVault IP Reputation Database data
#

setwd("~/Dropbox/datavizbook/bob-chapters/chapter3/ch3")
options(width=70)

# Downlad the data file to a local director for processing

# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes

avURL <- "http://reputation.alienvault.com/reputation.data"

# use relative path for the downloaded data
avRep <- "data/reputation.data"

# using an if{}-wrapped test with download.file() vs read.xxx()
# directly avoids having to re-download a 16MB file every time 
# we run the script

if (file.access(avRep)) {
  download.file(avURL, avRep) 
}

# read in the IP reputation db into a data frame
# this data file has no header, so set header=FALSE
av <- read.csv(avRep,sep="#",header=FALSE)

# assign more readable column names since we don't 
# have a header for R to learn them from
colnames(av) <- c("IP","Reliability","Risk","Type",
                  "Country","Locale","Coords","x")

# get an R overview of the data frame with str()
str(av)

# take a quick look at the first few rows of data
head(av)

# get an overview of each individual column
summary(av$Reliability)
summary(av$Risk)

# since these are integers, we can create a table()
table(av$Reliability)
table(av$Risk)

# summary sorts by the counts by default
# maxsum sets how many factors to display
summary(av$Type, maxsum=10)
summary(av$Country, maxsum=40)


# Bar graph of counts (sorted) by Country (top 20)
# get the top 20 countries' names
country.top20 <- names(summary(av$Country))[1:20]

# give ggplot a subset of our data (the top 20 countries) 
# map the x value to a sorted count of country
g <- ggplot(data=subset(av,Country %in% country.top20), 
            aes(x=reorder(Country,Country,length)))
# tell ggplot we want a bar chart and to sort by country count
g <- g + geom_bar()
# ensure we have decent labels
g <- g + labs(title="Country Counts", x="Country")
# rotate the chart to make this one more readable
g <- g + coord_flip()
print(g)

# Bar graph of counts by Risk
# note we can call ggplot and add the bar chart in one line
g <- ggplot(data=av, aes(x=Risk)) + geom_bar()
# force an X scale to be just the limits of the data
# and to be discrete vs continuous
g <- g + scale_x_discrete(limits=seq(max(av$Risk)))
g <- g + labs(title="'Risk' Counts", x="Risk Score")
print(g)

# Bar graph of counts by Reliability
g <- ggplot(data=av, aes(x=Reliability)) + geom_bar()
g <- g + scale_x_discrete(limits=seq(max(av$Reliability)))
g <- g + labs(title="'Reliabiity' Counts", x="Reliability Score")
print(g)


# store the top 10 returned by summary() in a vector
country10 <- summary(av$Country, maxsum=10)
# now convert to a percentage by dividing by number of rows 
country.perc10 <- country10/nrow(av)
# and print it
print(country.perc10)


# cross-tabulate the Risk & Reliability factors
rr.tab <- xtabs(~Risk+Reliability, data=av)
ftable(rr.tab) # print table

# graphical view
library(lattice)
# cast the table into a data frame
rr.df = data.frame(table(av$Risk, av$Reliability))
# set the column names since table uses "Var1" and "Var2"
colnames(rr.df) <- c("Risk", "Reliability", "Freq")
# now create a level plot with readable labels
levelplot(Freq~Risk*Reliability, data=rr.df, main="Risk ~ Reliabilty", 
          ylab="Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#FFFFFF", "#0868AC"))(20))


# generate random samples for risk & reliability and re-run xtab
# starting PRNG from reproducable point
set.seed(1492) # as it leads to discovery
# generate 260,000 random samples
rsk=sample(1:7, 260000, replace=T)
rel=sample(1:10, 260000, replace=T)
# cast table into data frame
tmp.df = data.frame(table(factor(rsk), factor(rel)))
colnames(tmp.df) <- c("Risk", "Reliability", "Freq")
levelplot(Freq~Reliability*Risk, data=tmp.df, main="Risk ~ Reliabilty", 
          ylab="Risk", xlab = "Reliability", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#FFFFFF", "#0868AC"))(20))

# create a new varible called "simpletype" 
# replacing mutiple categories with label of "Multiples"
av$simpletype <- as.character(av$Type)
# Group all nodes with mutiple categories into a new category
av$simpletype[grep(';', av$simpletype)] <- "Multiples"
# Turn it into a factor again
av$simpletype <- factor(av$simpletype)

rrt.df = data.frame(table(av$Risk, av$Reliability, av$simpletype))
colnames(rrt.df) <- c("Risk", "Reliability", "simpletype", "Freq")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))

# from the existing rrt.df, filter out ‘Scanning Host’
rrt.df <- subset(rrt.df, simpletype != "Scanning Host")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))

# filter out malware dist & domain
rrt.df = subset(rrt.df, 
           !(simpletype %in% c("Malware distribution", 
                               "Malware Domain")))
print(sprintf("Count: %d; Percent: %2.1f%%",
              sum(rrt.df$Freq),
              100*sum(rrt.df$Freq)/nrow(av)))
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))
