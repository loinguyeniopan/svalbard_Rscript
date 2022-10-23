library(vegan)

pc = read.csv("abundance.csv",row.names = 1, header= TRUE)

pc = read.csv("abundance_meta.csv",row.names = 1, header= TRUE)

#make community matrix - extract columns with abundance information
com = pc[,4:ncol(pc)]

#turn abundance data frame into a matrix
m_com = as.matrix(com)

#normalize data
library(labdsv)
m_com <- hellinger(m_com)

library(cluster)
m_com <- daisy(m_com, metric="manhattan")
m_com <- vegdist (m_com, metric="manhattan")

#Convert to relative frequencies
m_com<-m_com/rowSums(m_com)

#Convert to relative frequencies
m_com<- log1p(m_com)

set.seed(123)

ano = anosim(m_com1, pc$wm, distance = "bray", permutations = 999)
ano



#nmds
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds

plot(nmds)

plot(nmds, type = "t", display= c("sites"))

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

data.scores = as.data.frame(scores(nmds)$sites)

#add columns to data frame 
data.scores$station = pc$station
data.scores$name = pc$name
data.scores$location = pc$location



head(data.scores)

library(ggplot2)
library(ggrepel)
library(dplyr)

#ve duong elips
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}


#data for ellipse, in this case using the management factor
df_ell.location <- data.frame() #sets up a data frame before running the function.
for(g in levels(data.scores$location)){
  df_ell.location <- rbind(df_ell.location, cbind(as.data.frame(with(data.scores [data.scores$location==g,],
      veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,location=g))
}


xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes( colour = location))+
  geom_text_repel(aes(x = NMDS1, y = NMDS2, label = name), size = 4) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Location", y = "NMDS2", shape = "station")  + 
  scale_colour_manual(values = c("#009E73", "#E69F00" , "#CC0000", "#006600", "#669999"))

xx1 = xx + geom_path(data = df_ell.location, aes(x = NMDS1, y = NMDS2, group = location, color = location), size=1, linetype=2)

xx1

NMDS1 <-  data.frame(data.scores$NMDS1)
NMDS2 <-  data.frame(data.scores$NMDS2)

install.packages("pheatmap")
library("pheatmap")
pheatmap(com, scale = "row")

heat <- t(as.matrix(m_com))
pheatmap(cor(heat,method="spearman"), clustering_method="ward.D2" )


mrpp( m_com, pc$wm, permutations = 999, distance = "bray")
