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
  download.file(avURL,avRep) 
}

# read in the IP reputation db into a data frame
# this data file has no header, so set header=FALSE
av <- read.csv(avRep,sep="#",header=FALSE)

# assign more readable column names since we don't 
# have a header for R to learn them from
colnames(av) <- c("IP","Reliability","Risk","Type",
                  "Country","Locale","Coords","x")

# take a quick look at the first 10 rows of data
head(av,n=10)

# get an R overview of the data frame
summary(av)

# as we'll be looking at the data quite a bit, 
# define a function to combine some common tasks
# set length=10 to limit output without having
# to always specify the parameter
headSummary <- function(x, length=10) {
  # count and organize the data
  x.factor <- summary(factor(x)) 
  # sort the table in descending order
  x.sorted <- sort(x.factor, decreasing=TRUE) 
  # show the first length values
  head(x.sorted, n=length)  
}

# get an overview of each individual column
summary(factor(av$Reliability))
summary(factor(av$Risk))
headSummary(av$Type)
headSummary(av$Country,length=Inf)


# Bar graph of counts (sorted) by Country (top 20)
# get the top 20 countries' names
country.top20 <- names(headSummary(av$Country,length=20))
# create a subset of the av data frame by selecting only the top
# 20 countries (by name)
# tell ggplot what data we are using
g <- ggplot(data=subset(av,Country %in% country.top20))
# tell ggplot we want a bar chart and to sort by country count
g <- g + geom_bar(aes(reorder(Country,Country,length)))
# ensure we have decent labels
g <- g + labs(title="Country Counts",x="Country")
# rotate the chart to make this one more readable
g <- g + coord_flip()
g

# Bar graph of counts by Risk
g <- ggplot(data=av[])
g <- g + geom_bar(aes(Risk))
# force an X scale to be just the limits of the data
# and to be discrete vs continuous
g <- g + scale_x_discrete(limits=sequence(range(av$Risk)))
g <- g + labs(title="'Risk' Counts",x="Risk Score")
g

# Bar graph of counts by Reliability
g <- ggplot(data=av)
g <- g + geom_bar(aes(Reliability))
g <- g + scale_x_discrete(limits=sequence(range(av$Reliability)))
g <- g + labs(title="'Reliabiity' Counts",x="Reliability Score")
g


# make a table out of the Country factor
country.table = table(av$Country)
# get the top 10 countries
top10 = names(sort(country.table,decreasing=TRUE))[1:10]
# calculate the % for each of the top 5
sapply(top10,function(x) {
  country.table[names(country.table)==x]/length(av$Country)
})


# cross-tabulate the Risk & Reliability factors
rr.tab <- xtabs(~Risk+Reliability, data=av)
ftable(rr.tab) # print table

# graphica view
library(lattice)
rr.df = data.frame(table(av$Risk, av$Reliability))
levelplot(Freq~Var2*Var1, data=rr.df, main="Risk ~ Reliabilty", 
          ylab="Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))


# generate random samples for risk & reliability and re-run xtab
rsk=sample(1:7,260000,replace=T)
rel=sample(1:10,260000,replace=T)
tmp.df = data.frame(table(factor(rsk), factor(rel)))
levelplot(Freq~Var2*Var1, data=tmp.df, main="Risk ~ Reliabilty", 
          ylab="Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))

# create a new "Type" from nodes with multipe Types
av$newtype <- as.character(av$Type)
av$newtype[grep(';', av$newtype)] <- "Multiples"
av$newtype = factor(av$newtype)
rrt.df = data.frame(table(av$Risk, av$Reliability, av$newtype))

levelplot(Freq ~ Var2*Var1|Var3, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))

# fiter out "scanning host"s
rrt.df = subset(rrt.df, Var3 != "Scanning Host")
rrt.df$Var3 = factor(rrt.df$Var3)
levelplot(Freq ~ Var2*Var1|Var3, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))

# filter out malware dist & domain
rrt.df = subset(rrt.df, 
           !(Var3 %in% c("Malware distribution", "Malware Domain")))
rrt.df$Var3 = factor(rrt.df$Var3)
sprintf("Count: %d; Percent: %2.1f%%",
        sum(rrt.df$Freq),
        100*sum(rrt.df$Freq)/nrow(av))
levelplot(Freq ~ Var2*Var1|Var3, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#FFFFFF","#0868AC"))(20))