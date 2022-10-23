library(vegan)
library(ggplot2)


pc = read.csv("random2.csv",row.names = NULL, header= TRUE)

m_com = data.frame(pc)
head(pc)

p1 <- ggplot(data=m_com, aes(x=station, y=read, fill=class)) + 
  geom_bar(stat="identity", position = "fill") + theme_bw() + facet_wrap(~method)
p1 <- p1 + scale_x_discrete(limits=c("IS1", "IS2", "WIJ1", "WIJ3", "RIJ1", "RIJ2", "RIJ3", "RIJ4", "NAL4", "NAL5", "NAL6", "NAL7", "EDG1", "EDG2", "EDG3"))
p1 <- p1 + labs(x="", y = "Reads proportion") + scale_y_continuous(labels = scales::percent)
p1 <- p1 + theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.text.y = element_text(face="bold", color="#000000"),
                 axis.title.y = element_text(color="#000000", face="bold"))

p1 <- p1 + theme(strip.background = element_blank(),
                 strip.text.x = element_text(size = 10,face="bold" ))

p1 <- p1 + scale_fill_discrete(name = "")
p1 

p2 <- ggplot(data=m_com, aes(x=station, y=asvs, fill=class)) + 
  geom_bar(stat="identity", position = "fill") + theme_bw() + facet_wrap(~method)
p2 <- p2 + scale_x_discrete(limits=c("IS1", "IS2", "WIJ1", "WIJ3", "RIJ1", "RIJ2", "RIJ3", "RIJ4", "NAL4", "NAL5", "NAL6", "NAL7", "EDG1", "EDG2", "EDG3"))
p2 <- p2 + labs(x="", y = "ASVs proportion") + scale_y_continuous(labels = scales::percent)
p2 <- p2 + theme(axis.text.x = element_text(size=7, face="bold", color="#000000"),
                 axis.text.y = element_text(face="bold", color="#000000"),
                 axis.title.x = element_text(color="#000000", face="bold"),
                 axis.title.y = element_text(color="#000000", face="bold"))
p2 <- p2 + theme(strip.background = element_blank(),
                 strip.text.x =  element_blank())
p2 


grid.arrange(p1, p2, nrow = 2)