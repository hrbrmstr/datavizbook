nf.srv <- read.csv("python/nf-chunk-ts.csv", header=T)
nf.srv <- read.csv("python/srv-1hr-ts.csv", header=T)
# srv-1hr-ts.csv
nf.o <- read.csv("python/nf-nosrv-ts.csv", header=T)
nf.srv <- read.csv("python/other-1hr-ts.csv", header=T)
#names(nf) <- c("ip", "ts", "sent", "recv")
#summary(nf$ts)
#hist(nf$ts)
#nf$realts <- nf$ts - min(nf$ts) + 1
#hist(nf$realts, breaks="scott")
# 
#print((max(nf$realts)/12)/24)
#summary(nf$ip, maxsum=10)

nf.srv$realts <- nf.srv$ts - min(nf.srv$ts) + 1
nf.o$realts <- nf.o$ts - min(nf.o$ts) + 1
fullts <- data.frame(realts=seq(max(nf.srv$realts)), realcount=0)

srv.sum <- aggregate(cbind(sent, recv) ~ realts, data=nf.srv, FUN=sum)
o.sum <- aggregate(cbind(sent, recv) ~ realts, data=nf.o, FUN=sum)

srv.merge <- merge(srv.sum, fullts, all=T)
srv.merge$sent[is.na(srv.merge$sent)] <- 0
srv.merge$recv[is.na(srv.merge$recv)] <- 0
o.merge <- merge(o.sum, fullts, all=T)
o.merge$sent[is.na(o.merge$sent)] <- 0
o.merge$recv[is.na(o.merge$recv)] <- 0

srv.sent <- ts(srv.merge$sent)
plot(srv.sent)
srv.recv <- ts(srv.merge$recv)
plot(srv.recv)
o.sent <- ts(o.merge$sent)
plot(o.sent)
o.recv <- ts(o.merge$recv)
plot(o.recv)

source("~/Documents/school/stat510/itall.R")


