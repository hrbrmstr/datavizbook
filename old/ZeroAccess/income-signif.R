library(ggplot2)
library(scales)
library(splines)

zero <- read.csv("zip-pop-inc-with-NA.csv", header=T)

# this next line replaces NA with zero and copies into "pop" vector
pop <- ifelse(is.na(zero$BotCount), 0, zero$BotCount)/zero$Population
# instead of rounding, we can set the "by" field to change range of bins
breaks <- seq(from=0, to=max(zero$MedianIncome)+10000, by=10000)
incomes <- cut(zero$MedianIncome, breaks, labels=breaks[2:length(breaks)])

z <- data.frame(inc = zero$MedianIncome, 
                pop = pop,
                inc.split = incomes,
                fullpop = zero$Population)

y <- z[z$pop>0,]  # I wanted to drop zeros from this.

qplot(inc, pop, data=y, xlab="Median Income", main="Income vs. ZeroAccess",
      ylab = "% of Population with ZeroAccess", colour=I(alpha("black",1/2))) + 
        geom_point(size=3, colour="#FF0000", alpha=I(1/10)) +
        geom_smooth(method=lm) +
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) +
        theme_bw()
# note on the above: "qplot" will plot points, that's why the plot is 
# black and red, see below for ggplot use

fit <- lm(pop ~ inc, data = z)
# summary of linear regression:
summary(fit)
# notice the line with "inc" and the "Pr(>|t|)" value (0.334)
# also notice the bottom "F-statistic", 
#   same p-value with single varaible regression model

# kind of a cool jitter plot showing bins of income
ggplot(y, aes(inc.split, pop)) +
        opts(title="Income vs. ZeroAccess Bot Infections") +
        geom_jitter(size=3, colour="#000000", alpha=I(1/10)) +
        ylab("% of Population with ZeroAccess") +
        scale_x_discrete("Median Income", labels=
          function(x) { paste(as.numeric(x)/1000, "k", sep="") }) +
        scale_y_continuous(trans = log2_trans(),
                           breaks = trans_breaks("log2", function(x) 2^x),
                           labels = trans_format("log2", math_format(2^.x))) + 
                             theme_bw()

# just doing income versus population for the fun of it.
qplot(inc, fullpop, data=z, xlab="Median Income", main="Income vs. Population",
      ylab = "Population within zipcode", colour=I(alpha("black",1/4))) + 
        geom_point(size=2, colour="#FF0000", alpha=I(1/15)) +
        geom_smooth(method=lm) +
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) +
                        theme_bw()

