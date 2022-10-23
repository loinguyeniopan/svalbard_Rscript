library(tidyverse)
library(broom)
library(ggplot2)
library(vegan)


#read data 

pc = read.csv("statest.csv",row.names = 1, header= TRUE)

#make community matrix - extract columns with abundance information
com = pc[,20:ncol(pc)]

#turn abundance data frame into a matrix
m_com = as.matrix(com)


#envfit
env = pc[,11:19]

en = envfit(nmds, env, permutations = 999, na.rm = TRUE)
en

en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)


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

ano = anosim(m_com, pc$method, distance = "bray", permutations = 999)
ano

env = pc[,11:19]

adonis2(m_com ~ method*location*depth, data = env, method = "bray", permutations = 999)


mrpp(m_com, pc$location, permutations = 999, distance = "bray")

#nmds
set.seed(123)
nmds = metaMDS(m_com, autotransform = F, distance = "bray")
nmds

plot(nmds)

plot(nmds, type = "t", display= c("sites"))
plot(en)

#extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

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
df_ell <- data.frame() #sets up a data frame before running the function.
for(g in levels(data.scores$location)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(data.scores [data.scores$location==g,],
veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2))))) ,station=g))
}

# data for labelling the ellipse
NMDS.mean.dune=aggregate(data.scores[ ,c("NMDS1", "NMDS2")], 
                         list(group = data.scores$location), mean)

# data for labelling the ellipse
NMDS.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                    list(group = data.scores$location), mean)

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
  labs(x = "NMDS1", colour = "location", y = "NMDS2", shape = "station")  + 
  scale_colour_manual(values = c("#009E73", "#E69F00" , "#CC0000", "#006600", "#669999"))
  
 xx1 = xx + geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2, group = location, color = location), size=1, linetype=2)


xx1 = xx1 + geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey0", arrow=arrow(length=unit(0.20,"cm"))) +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey0", 
            fontface = "bold", label = row.names(en_coord_cont))
  
  
NMDS=data.frame(x = data.scores[ ,1],y=data.scores[,2], location = as.factor(data.scores[,5]))




veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#Generate ellipse points
df_ell <- data.frame()
for(g in levels(NMDS$location)){
  if(g!="" && (g %in% names(ord))){
    
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$location==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale)))
                                  ,location=g))
  }
}

head(df_ell)

library("pheatmap")
pheatmap(com, scale = "row")

heat <- t(as.matrix(m_com))
pheatmap(cor(heat,method="spearman"), clustering_method="ward.D2" )


mrpp( m_com, pc$method, permutations = 999, distance = "bray")
mrpp( m_com, pc$location, permutations = 999, distance = "bray")

