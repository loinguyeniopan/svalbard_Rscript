library(vegan)

pc = read.csv("abundance.csv",row.names = 1, header= TRUE)
pc = read.csv("ASV_CSS.csv",row.names = 1, header= TRUE)

#make community matrix - extract columns with abundance information
com = pc[,4:ncol(pc)]

#turn abundance data frame into a matrix
m_com = as.matrix(com)

S <- specnumber(m_com) #Observed ASV

H1 <- diversity(m_com, "simpson")

write.csv(H1,"H1.csv")

vegan.out <-estimateR(m_com) #Chao1




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

summary(ano)

#nmds
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")

nmds = metaMDS(m_com, autotransform = F, distance = "bray")
nmds

plot(nmds)

plot(nmds, type = "t", display= c("sites"))

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
  
xx

xx1


xx = ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 3, aes( colour = location))+
  geom_text_repel(aes(x = NMDS1, y = NMDS2, label = name), size = 4) +
  theme(axis.text.y = element_text(colour = "black", size = 12, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 12), 

        axis.title.x = element_text(face = "bold", size = 14, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Location", y = "NMDS2", shape = "station")  + 
  scale_colour_manual(values = c("#009E73", "#E69F00" , "#CC0000", "#006600", "#669999"))+
  theme(legend.position = "none")

rlang::last_error()

install.packages("pheatmap")
library("pheatmap")
pheatmap(com, scale = "row")

heat <- t(as.matrix(m_com))
pheatmap(cor(heat,method="spearman"), clustering_method="ward.D2" )


mrpp( m_com, pc$wm, permutations = 999, distance = "bray")


#(R version 3.5.3 (2019-03-11)
Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
  [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
  [1] dplyr_1.0.7     ggrepel_0.9.1   ggplot2_3.3.5   vegan_2.5-7     lattice_0.20-44 permute_0.9-5  

loaded via a namespace (and not attached):
  [1] Rcpp_1.0.7       pillar_1.6.2     compiler_3.5.3   tools_3.5.3      digest_0.6.27    lifecycle_1.0.0  tibble_3.1.3     gtable_0.3.0    
[9] nlme_3.1-152     mgcv_1.8-27      pkgconfig_2.0.3  rlang_0.4.11     Matrix_1.3-4     DBI_1.1.1        parallel_3.5.3   withr_2.4.2     
[17] cluster_2.1.2    generics_0.1.0   vctrs_0.3.8      grid_3.5.3       tidyselect_1.1.1 glue_1.4.2       R6_2.5.0         fansi_0.5.0     
[25] farver_2.1.0     purrr_0.3.4      magrittr_2.0.1   scales_1.1.1     ellipsis_0.3.2   MASS_7.3-54      splines_3.5.3    assertthat_0.2.1
[33] colorspace_2.0-2 labeling_0.4.2   utf8_1.2.2       munsell_0.5.0    crayon_1.4.1    )
> 
