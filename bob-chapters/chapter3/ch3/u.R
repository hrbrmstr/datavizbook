foo <- as.data.frame(table(av$Reliability, av$Risk))
colnames(foo) <- c("Reliability", "Risk", "Freq")
foosum <- sum(foo$Freq)
foo$Risk <- as.numeric(foo$Risk)
foo$Reliability <- as.numeric(foo$Reliability)
foo$prob <- apply(foo, 1, function(x) round(sum(foo$Freq[which(foo$Risk>x['Risk'] & foo$Reliability>x['Reliability'])])/foosum, 4)*100)
ggplot(foo, aes(Reliability, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=prob))


foo$tot <- apply(foo, 1, function(x) sum(foo$Freq[which(foo$Risk>x['Risk'] & foo$Reliability>x['Reliability'])]))
ggplot(foo, aes(Reliability, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=tot))

mycolors <- c("yellow1","yellow2","yellow3","yellow4", 
              "thistle1","thistle2","thistle3")
cont.table <- table(av$Reliability, av$Risk)
plot(cont.table, col=mycolors, main="Risk and Reliability")

av$newtype <- av$Type
av$newtype[grep(';', av$newtype)] <- "Multiples"
foo <- as.data.frame(table(av$newtype, av$Risk))
colnames(foo) <- c("Type", "Risk", "Freq")
ggplot(foo, aes(Type, Risk)) + geom_tile(aes(fill=Freq), colour="white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + geom_text(aes(label=Freq))


ggplot(foo[grep("Scanning Host",foo$Type,invert=TRUE),], 
       aes(Risk, Freq)) + geom_bar() + facet_grid(Type~.) + theme_few()

