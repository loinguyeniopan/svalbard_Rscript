library(tidyverse)
theme_set(theme_bw(16))

#import data

station = read.csv("cca.csv", row.names=1, header= TRUE)

foram <- data.frame(station)

foram %>% head()

X <- foram %>% 
  select(sal, temp, t_ftu,	depth_m) %>%
  scale()

Y <- foram %>%
  select(Foraminifera_X,	Globothalamea,	Monothalamea,	Tubothalamea
) %>%
  scale()
head(Y)

library(CCA)
cc_results <- cancor(X,Y)


str(cc_results)

cc_results$xcoef

cc_results$ycoef

cc_results$cor

CC1_X <- as.matrix(X) %*% cc_results$xcoef[, 1]
CC1_Y <- as.matrix(Y) %*% cc_results$ycoef[, 1]

CC2_X <- as.matrix(X) %*% cc_results$xcoef[, 2]
CC2_Y <- as.matrix(Y) %*% cc_results$ycoef[, 2]

cor(CC1_X,CC1_Y)

assertthat::are_equal(cc_results$cor[1], 
                      cor(CC1_X,CC1_Y)[1])

cca_df <- foram %>% 
  mutate(CC1_X=CC1_X,
         CC1_Y=CC1_Y,
         CC2_X=CC2_X,
         CC2_Y=CC2_Y)

cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y))+
  geom_point()

cca_df %>% 
  ggplot(aes(x=location,y=CC1_Y, color=location))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)


cca_df %>% 
  ggplot(aes(x=CC1_X,y=CC1_Y, color=location))+
  lims(x = c(-1, 1)) + lims(y = c(-1,1)) +  
  theme(panel.background = element_blank()) + geom_hline(aes(yintercept=0), 
                                                         colour="#8c8c8c") +
  geom_point()

