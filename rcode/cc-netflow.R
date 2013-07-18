foo <- read.csv("/home/jay/mac/json/sharable/reputation.data.txt", sep='#', header=F)

goo <- foo[which(foo$V4 %in% 'C&C'), ]
hoo <- goo[which(goo$V5=="US"), ]

# http://www.geonames.org/countries/
# http://countrycode.org/turkey

goo <- foo[which(foo$V4=="Spamming" & foo$V5=="US"), ]
outs <- as.character(hoo[which(hoo$V6=="Chicago"), 1])
paste(outs, collapse=",")


foo <- read.csv("/home/jay/mac/temp/GeoIPASNum2.csv", header=F)
nrow(foo)
foo$V4 <- foo$V2 - foo$V1
goo <- aggregate(V4 ~ V3, data=foo, FUN=sum)
nrow(goo)

head(goo[with(goo, order(V4, decreasing=T)), ])

IPtoDecimal <- function(ip) {
  foo <- strsplit(ip, '.', fixed=T)
  goo <- lapply(foo, as.numeric)
  hoo <- lapply(goo, "*", base.mult)
  sapply(hoo, sum)  
}

files <- c('kevin-12.38.236.32.csv', # 'kevin-198.143.231.107.csv',
           'kevin-198.199.78.132.csv', 'kevin-58.64.191.136.csv',
           'kevin-66.228.132.53.csv', 'kevin-67.229.53.75.csv',
           'ccchi-108.178.35.221.csv', 'ccchi-184.154.231.4.csv',
           'ccchi-204.93.207.232.csv', 'ccchi-204.93.207.246.csv',
           'ccchi-205.234.140.233.csv', 'ccchi-67.202.109.141.csv',
           'ccchi-69.175.127.59.csv', 'ccchi-69.175.84.194.csv',
           'ccchi-96.30.0.180.csv', 'spamny-199.30.138.46.csv',
           'spamny-66.199.236.101.csv')

f1 <- strsplit(files, "-")
ftypes <- sapply(f1, function(x) x[1] )
f2 <- strsplit(sapply(f1, function(x) x[2]), '.', fixed=T)
ips <- sapply(f2, function(x) paste(x[1:4], collapse="."))

mydata <- data.frame()
for (i in seq_along(files)) {
  print(files[i])
  single <- getACF(files[i], ftypes[i], ips[i])
  if(nrow(mydata)) {
    mydata <- rbind(mydata, single)
  } else {
    mydata <- single
  }
}

getACF <- function(fname, ftype, ip) {
  print(fname)
  print(ftype)
  print(ip)
  filename <- paste("~/mac/book/data/output/", fname, sep="")
  foo <- read.csv(filename, header=T)
  timestr <- strptime(foo$X..Start.Time, "%Y-%m-%d %H:%M:%S")
  bytes <- foo$Bytes
  fullts <- data.frame(t=timestr, sz=bytes)
  temp <- seq(fullts$t[1], fullts$t[nrow(fullts)], by=300)
  realdata <- rep(NULL, length(temp)-1)
  for(i in 2:length(temp)) {
    realdata[i-1] <- mean(fullts$sz[which(fullts$t>=temp[i-1] & fullts$t<temp[i])])
  }
  realts <- as.ts(ifelse(is.nan(realdata), 0, realdata))
  goober <- acf(realts)
  acfmean <- mean(as.numeric(unlist(goober)[1:39]))
  acfsd <- sd(as.numeric(unlist(goober)[1:39]))  
  data.frame(ip=ip, cl=ftype, acfmean=acfmean, acfsd=acfsd)
}

pacf(realts)
pacf(realts)[c(6,12)]

acf(realts)
