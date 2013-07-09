#
# reputation.R
#
# sample analysis script for AlienVault IP Reputation Database data
#

# URL for the AlienVault IP Reputation Database (OSSIM format)
# storing the URL in a variable makes it easier to modify later
# if it changes

avURL <- "http://reputation.alienvault.com/reputation.data"

# relative path for the downloaded data
avRep <- "data/reputation.data"

# using an if{}-wrapped test with download.file() vs read.xxx() avoids
# having to re-download a 16MB file every time we run the script

if (file.access(avRep)) {
  download.file(avURL,avRep) 
}

# read in the IP reputation db into a data frame
av <- read.csv(avRep,sep="#")

# take a quick look at the data
head(av)

# assign more readable column names to make it easier to work with the data 
# IP | reliability | risk | type | country | locale | coords | x
colnames(av) <- c("IP","Reliability","Risk","Type","Country","Locale","Coords","x")
