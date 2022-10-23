library(vegan)
library(ggplot2)

pc = read.csv("random.csv",row.names = NULL, header= TRUE)

m_com = data.frame(pc)


# Basic barplot

# observed_otus

p1<- ggplot(data=m_com, aes(x=station, y=observed_otus, fill=method)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+ theme(legend.position="none")
  p1 <- p1 + scale_x_discrete(limits=c("IS1", "IS2", "WIJ1", "WIJ3", "RIJ1", "RIJ2", "RIJ3", "RIJ4", "NAL4", "NAL5", "NAL6", "NAL7", "EDG1", "EDG2", "EDG3"))
p1 <- p1 + labs(title="(A)", x="", y = "Observed ASVs") + scale_y_continuous(breaks=seq(0,600,100))
p1 <- p1 + theme(axis.text.x = element_text(size=8, face="bold", color="#000000"),
                axis.text.y = element_text(face="bold", color="#000000"),
                axis.title.x = element_text(color="#000000", face="bold"),
                axis.title.y = element_text(color="#000000", face="bold"))

p1
# chao1
p2<- ggplot(data=m_com, aes(x=station, y=chao1, fill=method)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_classic() + theme(legend.position="none")
p2 <- p2 + scale_x_discrete(limits=c("IS1", "IS2", "WIJ1", "WIJ3", "RIJ1", "RIJ2", "RIJ3", "RIJ4", "NAL4", "NAL5", "NAL6", "NAL7", "EDG1", "EDG2", "EDG3"))
p2 <- p2 + labs(title="(B)", x="", y = "Chao1 Index") + scale_y_continuous(breaks=seq(0,600,100))
p2 <- p2 + theme(axis.text.x = element_text(size=8, face="bold", color="#000000"),
                axis.text.y = element_text(face="bold", color="#000000"),
                axis.title.x = element_text(color="#000000", face="bold"),
                axis.title.y = element_text(color="#000000", face="bold"))
p2

# shannon
p3<- ggplot(data=m_com, aes(x=station, y=shannon1, fill=method)) + 
  geom_bar(stat="identity", position=position_dodge()) + theme_classic() +theme(legend.position="none")
p3 <- p3 + scale_x_discrete(limits=c("IS1", "IS2", "WIJ1", "WIJ3", "RIJ1", "RIJ2", "RIJ3", "RIJ4", "NAL4", "NAL5", "NAL6", "NAL7", "EDG1", "EDG2", "EDG3"))
p3 <- p3 + labs(title="(C)", x="", y = "Shannon Index") + scale_y_continuous(breaks=seq(0,7,1))
p3 <- p3 + theme(axis.text.x = element_text(size=8, face="bold", color="#000000"),
                 axis.text.y = element_text(face="bold", color="#000000"),
                 axis.title.x = element_text(color="#000000", face="bold"),
                 axis.title.y = element_text(color="#000000", face="bold"))
p3

# simpson
p4<- ggplot(data=m_com, aes(x=station, y=simpson1, fill=method)) +
  geom_bar(stat="identity", position = "dodge") + theme_classic() +theme(legend.position="none")
p4 <- p4 + scale_x_discrete(limits=c("IS1", "IS2", "WIJ1", "WIJ3", "RIJ1", "RIJ2", "RIJ3", "RIJ4", "NAL4", "NAL5", "NAL6", "NAL7", "EDG1", "EDG2", "EDG3"))
p4 <- p4 + labs(title="(D)", x="", y = "Simpson Index")

p4 <- p4 + theme(axis.text.x = element_text(size=8, face="bold", color="#000000"),
                 axis.text.y = element_text(face="bold", color="#000000"),
                 axis.title.x = element_text(color="#000000", face="bold"),
                 axis.title.y = element_text(color="#000000", face="bold"))

p4

grid.arrange(p1, p2, p3, p4, nrow = 3)

require(gridExtra)

plot(p)

head(pc)