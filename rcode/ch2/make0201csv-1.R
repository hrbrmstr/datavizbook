# figure 2.1

library(ggplot2)
library(scales)
library(RColorBrewer)

getMonth <- function() {
  basep <- c(.22, .16, .14, .12, .10, .08, .08, .10)
  adj <- round(rnorm(8, sd=.03), 3)
  single.month <- basep + adj
  single.adj <- (1-sum(single.month))/8
  single.month <- single.month + single.adj
  if (length(which(single.month<0))) {
    single.month <- getMonth()
  }
  single.month
}
labels <- c("Prescriptions", "Travel", "Adult", "Education", "Other Products", 
            "Health", "Internet", "Misc")
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
foo <- replicate(12, getMonth(), simplify="vector")
spam <- data.frame(p=as.vector(foo), month=rep(month, each=8), type=rep(labels, 12))
spam$month <- factor(spam$month, levels=month, ordered=T)
spam$type <- factor(spam$type, levels=labels, ordered=T)

#colours <- brewer.pal(8, "RdBu")
colours <- rev(brewer.pal(9, "PuBuGn")[-1])

gg <- ggplot(spam, aes(month, p, fill=type)) + 
  geom_bar(stat="identity") + scale_fill_manual(values = colours) +
  scale_y_continuous(labels = percent_format(), expand=c(0,0), breaks=seq(.1, .9, by=.1)) +
  theme_bw() + guides(fill = guide_legend(reverse=TRUE)) +
  theme(
    legend.background = element_blank(),
    legend.key        = element_blank(),
    legend.title       = element_blank(),
    panel.background  = element_blank(),
    panel.border      = element_blank(),
    strip.background  = element_blank(),
#    plot.background   = element_blank(),
    axis.line         = element_blank(),
    axis.line.x      = element_blank(),
#    axis.title.x      = element_blank(),
    axis.title  = element_blank(),
    axis.ticks = element_blank(),
#    axis.text.x      = element_blank(),
    panel.grid = element_blank())

print(gg)