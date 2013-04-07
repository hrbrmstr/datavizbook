library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(RColorBrewer)

# read in the data file of missing patches
df = read.csv("patch.csv")

# setup common plot parameters

base = ggplot(data=df) + 
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  theme_few()

# setup row 1 bubble and square plots

bubbles = base + 
  geom_point(aes(x=SBU,y=1,size=Missing.Critical.Patch.Count,fill="black")) + 
  facet_grid(Patch.OS~.) +
  scale_area(range=c(1,25)) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

squares = base + 
  geom_point(aes(x=SBU,y=1,size=Missing.Critical.Patch.Count,fill="black"),shape=15) + 
  facet_grid(Patch.OS~.) +
  scale_area(range=c(1,25)) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

# setup row 2 colored bubble and square plots

colored.bubbles = base + 
  geom_point(aes(x=SBU,y=1,size=Missing.Critical.Patch.Count,color=Patch.OS)) + 
  facet_grid(Patch.OS~.) +
  scale_area(range=c(1,25)) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

colored.squares = base + 
  geom_point(aes(x=SBU,y=1,size=Missing.Critical.Patch.Count,color=Patch.OS),shape=15) + 
  facet_grid(Patch.OS~.) +
  scale_area(range=c(1,25)) +
  theme(legend.position = "none", 
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

# setup row 3 line charts

sbu.lines = base + 
  geom_line(aes(x=SBU,y=Missing.Critical.Patch.Count,group=Patch.OS,color=Patch.OS))

os.lines = base + 
  geom_line(aes(x=Patch.OS,y=Missing.Critical.Patch.Count,group=SBU,color=SBU)) + 
  facet_grid(SBU~.) +
  theme(legend.position = "none")

# setup row 4 bar charts

sbu.bars = base +
  geom_bar(aes(x=SBU,y=Missing.Critical.Patch.Count,fill=Patch.OS),position="dodge")

os.bars = base +
  geom_bar(aes(x=Patch.OS,y=Missing.Critical.Patch.Count,fill=SBU),position="dodge")

# plot the charts

grid.arrange(bubbles,squares,
             colored.bubbles,colored.squares,
             sbu.lines,os.lines,
             sbu.bars,os.bars,ncol=2)

