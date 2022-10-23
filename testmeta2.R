library(ggplot2)
library(devtools)
library(cowplot)

pc = read.csv("asvread.csv",row.names = 1, header= TRUE)
pc1 = as.data.frame.matrix(pc)

pc = read.csv("rawcount.csv",row.names = 1, header= TRUE)
pc2 = as.data.frame.matrix(pc)
#color
c31 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown", 'cadetblue1',
  "darkolivegreen1" ,"tan2" ,   "tomato3" , "#7CE3D8","gainsboro")


p1<-ggplot(data=pc1, aes(x=read, y=station, fill=order)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c31)+
  theme_minimal()
p1 <- p1 + scale_y_discrete(limits=c( "EDG3", "EDG2", "EDG1", "NAL7", "NAL6","NAL5", "NAL4", "RIJ4", "RIJ3", "RIJ2", "RIJ1", "WIJ3", "WIJ2", "WIJ1", "IS3", "IS2", "IS1", "BALS", "HJEL", "RAU"))
p1 

p2<-ggplot(data=pc2, aes(x=read, y=station, fill=order)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c25)+
  scale_x_reverse ()+ # change the x axis label position
  theme_minimal()
p2 <- p2 + scale_y_discrete(limits=c( "EDG3", "EDG2", "EDG1", "NAL7", "NAL6","NAL5", "NAL4", "RIJ4", "RIJ3", "RIJ2", "RIJ1", "WIJ3", "WIJ2", "WIJ1", "IS3", "IS2", "IS1", "BALS", "HJEL", "RAU"))
p2
plot_grid(p2, p1, labels=c("A", "B"), ncol = 2, nrow = 1)
