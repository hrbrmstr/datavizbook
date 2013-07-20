census <- read.csv("~/mac/book/data/src/ip-to-census.csv", header=T)

census <- census[ ,4:9]
census$count[census$count==0] <- 1
size <- round(nrow(census)*.632)
train.index <- sample(nrow(census), size=size)
train <- census[train.index, ]
test <- census[-train.index, ]



model <- lm(count ~ ., data=train)
pred <- predict(model, test)

clean.census <- data.frame(ips=log(census$count), pop=log(census$pop))
model <- lm(ips ~ pop, data=clean.census)
summary(model)

smallcc <- clean.census[sample(nrow(clean.census), size=30), ]
model <- lm(ips ~ pop, data=smallcc)
CVlm(smallcc, model, m=10, plotit=TRUE)
summary(model)



