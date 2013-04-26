# Steven's Power Law probably distribution graph
# not sure it really conveys what I was hoping it would
# at least to the effect I was hoping to get
# meh


x = seq(0,100,by=1)

c = 1
y = c * (x^(-0.33)) # brightness, in dark
y1 = c * (x^(-0.5)) # brightness, point source
y2 = c * (x^(-0.7)) # visual areas, projected square
y3 = c * (x^(-1))   # length
y4 = c * (x^(-1.7)) # redness saturation
y5 = c * (x^(-3.5)) # electric shock

df = data.frame(x=x,y1,y2,y3,y4,y5)

ggplot(data=df, aes(x=x)) +
  geom_line(aes(y=y),color="orange") +
  geom_line(aes(y=y1),color="purple") +
  geom_line(aes(y=y2),color="darkgreen") + 
  geom_line(aes(y=y4),color="firebrick") +
  geom_line(aes(y=y5),color="goldenrod") +
  geom_line(aes(y=y3),color="black") +
  labs(x="Intensity",y="p(x)")
  
s_lines = ggplot(data=df, aes(x=x)) +
  geom_area(aes(y=y,color="orange", fill="orange"),position="stack") +
  geom_area(aes(y=y1,color="purple", fill="purple"),position="stack") +
  geom_area(aes(y=y2,color="darkgreen", fill="darkgreen"),position="stack") + 
  geom_area(aes(y=y3,color="black", fill="black"),position="stack") +
  geom_area(aes(y=y4,color="red",fill="red"),position="stack") +
  geom_area(aes(y=y5,color="gold", fill="gold"),position="stack") +
  labs(x="Intensity",y="p(x)")

s_area = ggplot(data=df, aes(x=x)) +
  geom_area(aes(y=y),color="orange", fill="orange") +
  geom_area(aes(y=y1),color="purple", fill="purple") +
  geom_area(aes(y=y2),color="darkgreen", fill="darkgreen") + 
  geom_area(aes(y=y3),color="black", fill="black") +
  geom_area(aes(y=y4),color="red",fill="red") +
  geom_area(aes(y=y5),color="gold", fill="gold") +
  labs(x="Intensity",y="p(x)",title="Stevens' Power Law Distributions") +
  theme(panel.background=element_blank())

ggsave(filename="~/Dropbox/datavizbook/figures/chapter\ 7/stevens.png",s_area,width=4,height=4)
