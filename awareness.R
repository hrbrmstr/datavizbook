#
# SUDA - Chapter 10 - Code for Figure 10.13
#
awareness.df <- data.frame(month=c("Mar","Apr","May","Jun","Jul","Aug"),
                           mnum=c(3,4,5,6,7,8),
                          measure=c(26,38,30,71,59,26))
bands.df <- data.frame(y.min=c(25,40,61,82,97), 
                       y.max=c(39,60,81,96,120),
                       labels=c("Low","Elev","Mod","Sig","High"))

gg <- ggplot(data=awareness.df)
gg <- gg + geom_hline(data=bands.df, aes(yintercept=y.max), color=c("#33A02C","#B2DF8A","#FF7F00","#FB9A99","#E31A1C"), size=0.5, alpha=0.5)
gg <- gg + labs(x="",y="",title="Security Awareness Risk")
gg <- gg + theme_bw() + theme(axis.ticks.y=element_blank(),
                              panel.background=element_blank(), 
                              panel.border=element_blank(),
                              panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              plot.background=element_blank())
gg <- gg + geom_bar(aes(x=reorder(month, mnum), y=measure), stat="identity", fill="goldenrod", width=0.5)
gg <- gg + scale_y_continuous(breaks=bands.df$y.max, labels=bands.df$labels)

gg
