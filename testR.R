library(vegan)
library(phyloseq)
library(tidyverse)
library(patchwork)

install.packages("patchwork")

install.packages("agricolae")

library(agricolae)

install.packages("FSA")

library(FSA)

install.packages("rcompanion")

library(rcompanion)


pc = read.csv("abundance.csv",row.names = 1, header= TRUE)


data_richness <- estimateR(pc)   

