show.students <- function() {
  gg <- ggplot(students, aes(x, y)) + geom_point(aes(fill=heads), shape=21, size=8, colour="#AAAAAA", linewidth=1) + 
    scale_fill_manual(values=c("#00AA00", "#00000000")) + theme_bw() +
    theme(legend.position = "none", 
          axis.line   = element_blank(),
          axis.title  = element_blank(), 
          axis.ticks  = element_blank(),
          axis.text   = element_blank(), 
          panel.background  = element_blank(),
          panel.border  = element_blank(), 
          panel.grid  = element_blank())
  print(gg)
}

getSample <- function(size) {
  newhead <- sample(c("fill", "nofill"),size=size, replace=TRUE, prob=c(0.5, 0.5) )
  if(length(which(newhead=="fill")) < size/2) {
    newhead <- getSample(size)
  }
  newhead
}

# set up the data frame
students <- data.frame(x=rep(seq(1:14), 16), y=rep(seq(1:16), each=14), heads="fill")
students$heads <- factor(students$heads, levels=c("fill", "nofill"), ordered=T)

# now this get iterated over (doing it manually for fun)
show.students()
standing <- which(students$heads=="fill")
newhead <- getSample(size=length(standing))
print(length(which(newhead=="fill")))
students$heads[standing] <- newhead
