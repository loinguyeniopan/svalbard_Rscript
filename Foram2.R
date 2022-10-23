pc = read.csv("abundance2.csv",row.names = 1, header= TRUE)
com = pc

m_com = as.matrix(com)


#nmds
set.seed(123)
nmds = metaMDS(m_com, distance = "bray")
nmds

plot(nmds)

plot(nmds, type = "t", display= c("sites"))

H <- diversity(pc)
simp <- diversity(pc, "simpson")
write.csv(H,"shanon.csv")
write.csv(simp,"simp.csv")

if(!require('phyloseq')) {
  install.packages('phyloseq')
  library('phyloseq')
}

data("esophagus")
estimate_richness(pc, measures=c("Observed",  "Shannon", "Chao1"))
class(pc)

pc = read.csv("abundance2.csv",row.names = 1, header= TRUE)

pc = otu_table(pc, taxa_are_rows = TRUE)


library(vegan)
library(dplyr)


pc = read.csv("abundance.csv" , header = TRUE)
otu = pc[,5:ncol(pc)]

foram <- data.frame(otu)

foram <- t(as.matrix(foram))

str(foram)

curve_all2 = specaccum(foram, method = "rarefaction", permutations = 100)


curve_all2 = specaccum(foram, method = "random", permutations = 1000)

plot(curve_all,add = TRUE )
#subset each habitat into its own df
pc %>% filter(location == "Edgeoya") -> coral
pc %>% filter(location == "Isfjorden") -> rubble


#calc species accumulation curve for each habitat
curve_coral = specaccum(coral[,5:ncol(pc)], method = "random")
curve_rubble = specaccum(rubble[5:ncol(pc)], method = "random")


#plot curve_all first
plot(curve_all2)
#then plot the rest
plot(curve_coral, add = TRUE, col = 2) #col is COLOUR setting, so change it to something else if you want
plot(curve_rubble, add = TRUE, col = 3)
plot(curve_sand, add = TRUE, col = 4)






