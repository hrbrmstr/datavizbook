library(ggplot2)
library(gridExtra)

set.seed(5)
x <- runif(200, min=0.0000001, max=2)
y <- log(x)*0.5 + rnorm(200, sd=0.2) + 2.2
plain <- ggplot(data.frame(x,y), aes(x, y)) + geom_point() + 
  theme_bw()
ggsave("figures/793725c06f003.eps", plain, width=7, height=5)
smooth <- ggplot(data.frame(x,y), aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x), size = 1, se=F) + 
  theme_bw()
ggsave("figures/793725c06f004.eps", smooth, width=7, height=5)

set.seed(1)
x <- runif(200, min=-10, max=10)
y <- 1.377*(x^3) + 0.92*(x^2) + .3*x + rnorm(200, sd=250) + 1572
x <- x + 10
# plain <- ggplot(data.frame(x,y), aes(x, y)) + geom_point() + 
#   theme_bw()
# ggsave("figures/793725c06f003.eps", plain, width=7, height=5)
smooth <- ggplot(data.frame(x,y), aes(x, y)) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), size = 1, se=F) + 
  theme_bw()
ggsave("figures/793725c06f003.eps", smooth, width=7, height=5)
x2 <- x^2
x3 <- x^3
model <- lm(y ~ x3 + x2 + x)
plot(model)

########## logistic
memproc <- read.csv("data/memproc.csv", header=T)
summary(memproc)
memproc$infected <- ifelse(memproc$state=="Infected", 1, 0)
set.seed(1492)
n <- nrow(memproc)
test.size <- as.integer(n/3)
testset <- sample(n, test.size)
test <- memproc[testset, ]
train <- memproc[-testset, ]

glm.out = glm(infected ~ proc + mem, data=test, family=binomial(logit))
summary(glm.out)
foo <- predict.glm(glm.out, test, type="response")
plot(foo, test$infected)
gg <- ggplot(data.frame(x=foo, y=ifelse(test$infected>0.5, "Yes", "No")), aes(x, y)) +
  geom_point(size=3, fill="steelblue", color="black", shape=4) + 
  ylab("Known Infected Host") +
  xlab("Estimated Probability of Infected Host") + theme_bw()
print(gg)
ggsave("figures/793725c06f004.eps", gg, width=7, height=2)
# estimate where?  0.4 for now.
goo <- ifelse(foo>0.4, 1, 0)
sum(goo==test$infected)/length(goo)
